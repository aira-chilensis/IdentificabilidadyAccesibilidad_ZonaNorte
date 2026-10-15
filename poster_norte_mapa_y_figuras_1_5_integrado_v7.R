# ==============================================================================
# MAPA DEL ÁREA DE ESTUDIO + FIGURAS 1–5
# PÓSTER CONGRESO DE LIDERAZGO CIENTÍFICO 2026
# Flora de humedales altoandinos del norte de Chile
#
# Mapa     Área de estudio: Arica y Parinacota a Coquimbo
# Fig. 1   Esquema conceptual de filtros
# Fig. 2   Diseño 2 × 2 de especies focales
# Fig. 3   Composición digital por especie
# Fig. 4   Complementariedad espacial
# Fig. 5   Accesibilidad territorial
#
# Integra la versión final v11 del mapa con el script reproducible
# del resto de las figuras.
# ==============================================================================

# ---- 0. Paquetes --------------------------------------------------------------
# Ejecutar una sola vez si falta alguno:
# install.packages(c(
#   "tidyverse",
#   "patchwork",
#   "sf",
#   "geodata",
#   "terra",
#   "ggspatial"
# ))

library(tidyverse)
library(patchwork)
library(grid)
library(sf)
library(geodata)
library(terra)
library(ggspatial)

# ---- 0.1. Carpetas de salida --------------------------------------------------
dir.create("figures", showWarnings = FALSE)
dir.create("analysis", showWarnings = FALSE)
dir.create("data_raw", showWarnings = FALSE)

# Nota de exportación:
# - Los PNG se guardan a 300 dpi y con fondo blanco.
# - Los PDF se exportan como formato preferente para maquetación/impresión.
# - El mapa descarga límites administrativos GADM mediante geodata la primera vez.
#

# ---- 0.2. Paleta y tema común -------------------------------------------------
# Paleta naturaleza, coherente entre figuras.
COL_HERB   <- "#4F7A5A"  # verde musgo
COL_INAT   <- "#B88A5A"  # tierra / café suave
COL_SHARED <- "#A8B99F"  # verde salvia
COL_LINE   <- "#7F9B78"  # verde grisáceo
COL_TEXT   <- "#202020"
COL_LIGHT  <- "#EEF2EC"

theme_poster <- theme_minimal(base_family = "sans", base_size = 13) +
  theme(
    text = element_text(colour = COL_TEXT),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11.5, hjust = 0.5, margin = margin(b = 16)),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, colour = COL_TEXT),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(14, 18, 14, 18),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA)
  )


# ---- 0.3. Paleta específica del mapa -----------------------------------------
# El área de estudio usa exactamente el mismo verde musgo que Herbario GBIF.
COL_STUDY  <- COL_HERB
COL_CHILE  <- "#D7D7D7"
COL_NEIGH  <- "#EFEFEF"
COL_BORDER <- "#5F5F5F"
COL_OCEAN  <- "#CFE7F1"
COL_GRID   <- "#A9C6D1"

# ==============================================================================
# MAPA DEL ÁREA DE ESTUDIO — versión final v11
# ==============================================================================

# ---- 3. Datos espaciales -------------------------------------------------------
# Regiones de Chile
chile_reg <- geodata::gadm(
  country = "CHL",
  level = 1,
  path = "data_raw"
) |>
  st_as_sf() |>
  st_transform(4326)

# Países vecinos como polígonos nacionales
get_country <- function(code) {
  geodata::gadm(
    country = code,
    level = 0,
    path = "data_raw"
  ) |>
    st_as_sf() |>
    st_transform(4326)
}

neighbors <- bind_rows(
  get_country("PER"),
  get_country("BOL"),
  get_country("ARG")
)

# ---- 4. Definir área de estudio ------------------------------------------------
chile_reg <- chile_reg |>
  mutate(
    NAME_1_clean = iconv(NAME_1, from = "", to = "ASCII//TRANSLIT"),
    in_study = NAME_1_clean %in% c(
      "Arica y Parinacota",
      "Tarapaca",
      "Antofagasta",
      "Atacama",
      "Coquimbo"
    )
  )

# Comprobación útil:
print(chile_reg |>
        st_drop_geometry() |>
        select(NAME_1, NAME_1_clean, in_study))

# ---- 5. Extensión del mapa -----------------------------------------------------
# Encuadre deliberado de Chile continental.
# Esto excluye Isla de Pascua, que era la principal razón de que el mapa anterior
# se abriera hacia el oeste y mostrara casi toda Sudamérica.
#
# Ajustes finos:
# - xmin más alto  -> menos mar
# - xmax más bajo  -> menos Argentina
# - modificar ancho/alto de ggsave -> figura más delgada o ancha
MAP_XLIM <- c(-78.8, -63.6)   # xmin algo más bajo: deja espacio al texto "Área de estudio"
MAP_YLIM <- c(-56.2, -14.5)   # ymax más al norte: el límite Perú–Bolivia queda dentro del encuadre

# ---- 6. Etiquetas manuales -----------------------------------------------------
# Posiciones elegidas para evitar superposición en el norte.
region_labels <- tibble::tribble(
  ~label,                   ~lon,   ~lat,
  "Arica y\nParinacota",    -70.35, -18.55,
  "Tarapacá",               -70.05, -20.35,
  "Antofagasta",            -69.55, -23.55,
  "Atacama",                -70.35, -27.25,
  "Coquimbo",               -71.05, -30.70
)

country_labels <- tibble::tribble(
  ~label,       ~lon,   ~lat,
  "PERÚ",       -72.00, -15.30,   # al oeste del límite Perú–Bolivia (dentro de Perú)
  "BOLIVIA",    -65.80, -17.00,   # al este del límite (dentro de Bolivia)
  "ARGENTINA",  -65.95, -35.05
)

# ---- 7. Figura -----------------------------------------------------------------
fig_map <- ggplot() +

  # Países vecinos
  geom_sf(
    data = neighbors,
    fill = COL_NEIGH,
    colour = "#B5B5B5",
    linewidth = 0.30
  ) +

  # Bordes de países vecinos (refuerza, por ejemplo, el límite Perú–Bolivia)
  geom_sf(
    data = neighbors,
    fill = NA,
    colour = "#7A7A7A",
    linewidth = 0.50
  ) +

  # Regiones de Chile
  geom_sf(
    data = chile_reg,
    aes(fill = in_study),
    colour = "white",
    linewidth = 0.38
  ) +

  # Contorno nacional de Chile, para que la costa se lea mejor
  geom_sf(
    data = chile_reg |> summarise(),
    fill = NA,
    colour = COL_BORDER,
    linewidth = 0.50
  ) +

  scale_fill_manual(
    values = c(
      "TRUE" = COL_STUDY,
      "FALSE" = COL_CHILE
    )
  ) +

  # Etiquetas del área de estudio
  geom_text(
    data = region_labels,
    aes(x = lon, y = lat, label = label),
    inherit.aes = FALSE,
    size = 2.65,
    fontface = "bold",
    colour = COL_TEXT,
    lineheight = 0.88
  ) +

  # Países vecinos
  geom_text(
    data = country_labels,
    aes(x = lon, y = lat, label = label),
    inherit.aes = FALSE,
    size = 3.00,
    colour = "#666666",
    family = "sans"
  ) +

  # Océano Pacífico
  annotate(
    "text",
    x = -75.45, y = -34.15,
    label = "Océano\nPacífico",
    size = 5.2,
    colour = "#2E6FA3",
    fontface = "italic",
    lineheight = 0.95
  ) +

  # Indicador simple del área de estudio
  annotate(
    "segment",
    x = -73.35, xend = -73.35,
    y = -30.8, yend = -18.1,
    linewidth = 0.7,
    colour = COL_STUDY
  ) +
  annotate(
    "segment",
    x = -73.35, xend = -72.95,
    y = -18.1, yend = -18.1,
    linewidth = 0.7,
    colour = COL_STUDY
  ) +
  annotate(
    "segment",
    x = -73.35, xend = -72.95,
    y = -30.8, yend = -30.8,
    linewidth = 0.7,
    colour = COL_STUDY
  ) +
  annotate(
    "text",
    x = -73.95, y = -24.45,        # termina antes del corchete (x = -73.35)
    label = "Área\nde estudio",
    size = 3.8,
    fontface = "bold",
    colour = COL_STUDY,
    hjust = 1,                     # alineado a la derecha: el borde derecho del texto queda a la izquierda del corchete
    lineheight = 0.9
  ) +

  # Escala
  annotation_scale(
    location = "bl",
    width_hint = 0.23,
    pad_x = unit(0.20, "cm"),
    pad_y = unit(0.20, "cm"),
    text_cex = 0.75,
    line_width = 0.45
  ) +

  # Norte
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.15, "cm"),
    pad_y = unit(0.18, "cm"),
    height = unit(0.6, "cm"),
    width  = unit(0.6, "cm"),
    style = north_arrow_fancy_orienteering
  ) +

  # Grilla del eje X (longitud) cada 4°: 76°W, 72°W, 68°W, 64°W
  scale_x_continuous(breaks = seq(-76, -64, by = 4)) +

  # Encuadre
  coord_sf(
    crs = 4326,
    xlim = MAP_XLIM,
    ylim = MAP_YLIM,
    expand = FALSE,
    datum = st_crs(4326)
  ) +

  labs(title = "Área de estudio") +

  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)   # separación entre el título y el borde superior del mapa (subir/bajar a gusto)
    ),

    # Mar
    panel.background = element_rect(
      fill = COL_OCEAN,
      colour = NA
    ),

    # Marco del mapa: se dibuja ENCIMA de las capas, así también se ve donde hay tierra
    # (arriba y a la derecha el borde quedaba tapado por Perú, Bolivia y Argentina)
    panel.border = element_rect(
      fill = NA,
      colour = COL_BORDER,
      linewidth = 0.55
    ),

    # Grilla geográfica
    panel.grid.major = element_line(
      colour = COL_GRID,
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),

    # Ejes/grilla exterior
    axis.title = element_blank(),
    axis.text = element_text(
      size = 9.5,
      colour = COL_TEXT
    ),
    axis.ticks = element_line(
      colour = COL_TEXT,
      linewidth = 0.35
    ),

    legend.position = "none",

    # Fondo blanco y margen exterior pequeño
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(
      t = 2,
      r = 2,
      b = 2,
      l = 2,
      unit = "mm"
    )
  )

# Mostrar en RStudio
fig_map

# ---- 8. Exportar ---------------------------------------------------------------
# Figura deliberadamente angosta para encajar bien en el póster vertical.
ggsave(
  "figures/Figura_mapa_area_estudio_chile_v11.png",
  fig_map,
  width = 5.4,
  height = 9.6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/Figura_mapa_area_estudio_chile_v11.pdf",
  fig_map,
  width = 5.4,
  height = 9.6,
  units = "in",
  bg = "white"
)



# ==============================================================================
# 1. DATOS MAESTROS
# ==============================================================================

species_master <- tribble(
  ~species,                    ~conspicuity,   ~identifiability, ~IOC, ~herbarium, ~inat,
  "Carex melanocystis",        "Conspicua",    "Difícil",         4,       9,          0,
  "Zameioscirpus atacamensis", "Conspicua",    "Difícil",         6,      13,          2,
  "Halerpestes exilis",        "Conspicua",    "Difícil",         4,       5,          1,
  "Oxychloe andina",           "Conspicua",    "Fácil",           6,      32,         14,
  "Deschampsia eminens",       "Conspicua",    "Fácil",           5,      42,          1,
  "Puccinellia frigida",       "Conspicua",    "Fácil",           4,      66,          2,
  "Arenaria rivularis",        "Inconspicua",  "Difícil",         0,       6,         10,
  "Arenaria serpens",          "Inconspicua",  "Difícil",         0,      19,          1,
  "Cinnagrostis velutina",     "Inconspicua",  "Difícil",         1,      38,          0,
  "Calandrinia compacta",      "Inconspicua",  "Fácil",           1,      26,          9,
  "Gamocarpha ventosa",        "Inconspicua",  "Fácil",           2,       2,          1,
  "Triglochin concinna",       "Inconspicua",  "Fácil",           1,      12,          2
) |>
  mutate(
    group = paste(conspicuity, identifiability, sep = " × "),
    total = herbarium + inat,
    p_herbarium = 100 * herbarium / total,
    p_inat = 100 * inat / total
  )

group_levels <- c(
  "Conspicua × Difícil",
  "Conspicua × Fácil",
  "Inconspicua × Difícil",
  "Inconspicua × Fácil"
)

species_levels <- c(
  "Carex melanocystis",
  "Zameioscirpus atacamensis",
  "Halerpestes exilis",
  "Oxychloe andina",
  "Deschampsia eminens",
  "Puccinellia frigida",
  "Arenaria rivularis",
  "Arenaria serpens",
  "Cinnagrostis velutina",
  "Calandrinia compacta",
  "Gamocarpha ventosa",
  "Triglochin concinna"
)

species_master <- species_master |>
  mutate(
    group = factor(group, levels = group_levels),
    species = factor(species, levels = species_levels)
  )

write_csv(species_master, "analysis/species_master_norte.csv")

# ==============================================================================
# FIGURA 1. ESQUEMA CONCEPTUAL DE FILTROS
# Cajas redondeadas en la paleta del póster, más compactas, flechas gruesas.
# ==============================================================================

# ---- Parámetros de formato (ajustables) ---------------------------------------
FIG1_WIDTH  <- 11.5
FIG1_HEIGHT <- 6.2
F1_ARROW_LW <- 1.6      # grosor de flechas
F1_PAD      <- 0.04     # separación flecha-caja

# Helper: caja redondeada dibujada en coordenadas de datos
rbox <- function(xmin, xmax, ymin, ymax, fill, border = NA, lwd = 2, r = 0.16) {
  annotation_custom(
    grob = grid::roundrectGrob(
      r  = grid::unit(r, "snpc"),
      gp = grid::gpar(fill = fill, col = border, lwd = lwd)
    ),
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
  )
}

# Cajas: centro (x, y), tamaño (w, h) y colores
boxes_f1 <- tribble(
  ~id,      ~x,    ~y,   ~w,   ~h,   ~label,                              ~fill,      ~border,   ~txt,       ~face,
  "flora",  1.000, 4.15, 1.45, 0.80, "Flora presente\nen el territorio",  "white",    COL_LINE,  COL_TEXT,   "plain",
  "access", 2.850, 4.15, 1.45, 0.80, "Accesibilidad\nterritorial",        COL_SHARED, COL_LINE,  COL_TEXT,   "plain",
  "detect", 4.700, 4.15, 1.45, 0.80, "Detección /\nconspicuidad",         COL_SHARED, COL_LINE,  COL_TEXT,   "plain",
  "ident",  6.550, 4.15, 1.45, 0.80, "Identificabilidad\ntaxonómica",     COL_SHARED, COL_LINE,  COL_TEXT,   "plain",
  "record", 3.775, 2.45, 2.00, 0.80, "Registro digital",                  COL_LIGHT,  COL_HERB,  COL_TEXT,   "bold",
  "herb",   2.600, 0.95, 1.90, 0.80, "Colecciones de\nherbario",          COL_HERB,   COL_HERB,  "white",    "bold",
  "inat",   4.950, 0.95, 1.90, 0.80, "iNaturalist /\nciencia ciudadana",  COL_INAT,   COL_INAT,  "white",    "bold"
) |>
  mutate(
    xmin = x - w / 2, xmax = x + w / 2,
    ymin = y - h / 2, ymax = y + h / 2
  )

box_layers <- lapply(seq_len(nrow(boxes_f1)), function(i) {
  b <- boxes_f1[i, ]
  rbox(b$xmin, b$xmax, b$ymin, b$ymax, fill = b$fill, border = b$border)
})

# Banda suave de fondo para los filtros previos
band_layer <- rbox(0.08, 7.47, 3.50, 5.10, fill = COL_LIGHT, border = NA, r = 0.07)

# Flechas superiores: borde a borde entre cajas contiguas
top <- boxes_f1 |> filter(id %in% c("flora", "access", "detect", "ident"))
seg_top <- tibble(
  x    = top$xmax[-nrow(top)] + F1_PAD,
  xend = top$xmin[-1]         - F1_PAD,
  y    = 4.15,
  yend = 4.15
)

arrow_f1 <- arrow(length = unit(0.45, "cm"), angle = 28, type = "closed")

# Conexión orgánica Identificabilidad -> Registro digital:
# curva de Bézier cúbica en "S". Sale vertical desde la base de la caja, se
# desliza suavemente hacia la izquierda y entra vertical en "Registro digital".
bezier_xy <- function(p0, p1, p2, p3, n = 120) {
  s <- seq(0, 1, length.out = n)
  tibble(
    x = (1 - s)^3 * p0[1] + 3 * (1 - s)^2 * s * p1[1] + 3 * (1 - s) * s^2 * p2[1] + s^3 * p3[1],
    y = (1 - s)^3 * p0[2] + 3 * (1 - s)^2 * s * p1[2] + 3 * (1 - s) * s^2 * p2[2] + s^3 * p3[2]
  )
}
curve_ident <- bezier_xy(
  p0 = c(6.55, 3.75 - F1_PAD),   # base de "Identificabilidad taxonómica"
  p1 = c(6.55, 3.25),            # control: arranca vertical
  p2 = c(4.30, 3.30),            # control: llega vertical
  p3 = c(4.30, 2.85 + F1_PAD)    # tope de "Registro digital"
)

fig1 <- ggplot() +
  band_layer +
  box_layers +

  # Flechas horizontales entre filtros
  geom_segment(
    data = seg_top,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = F1_ARROW_LW, colour = COL_HERB,
    lineend = "butt", arrow = arrow_f1
  ) +
  # Identificabilidad -> registro digital
  geom_path(
    data = curve_ident,
    aes(x = x, y = y),
    linewidth = F1_ARROW_LW, colour = COL_HERB,
    lineend = "butt", linejoin = "round", arrow = arrow_f1
  ) +
  # Registro digital -> fuentes
  geom_segment(
    aes(x = 3.05, y = 2.05 - F1_PAD, xend = 2.55, yend = 1.35 + F1_PAD),
    linewidth = F1_ARROW_LW, colour = COL_HERB,
    lineend = "butt", arrow = arrow_f1
  ) +
  geom_segment(
    aes(x = 4.50, y = 2.05 - F1_PAD, xend = 5.00, yend = 1.35 + F1_PAD),
    linewidth = F1_ARROW_LW, colour = COL_HERB,
    lineend = "butt", arrow = arrow_f1
  ) +

  # Texto de las cajas
  geom_text(
    data = boxes_f1,
    aes(x = x, y = y, label = label, colour = txt, fontface = face),
    size = 4.4, lineheight = 0.95
  ) +
  scale_colour_identity() +

  # Rótulos de sección
  annotate(
    "text", x = 3.775, y = 4.83,
    label = "Filtros previos al dato digital",
    size = 4.3, fontface = "bold", colour = COL_HERB
  ) +
  annotate(
    "text", x = 3.775, y = 1.70,
    label = "Fuentes digitales\nanalizadas",
    size = 4.2, fontface = "bold", colour = COL_HERB, lineheight = 0.95
  ) +

  coord_cartesian(xlim = c(0, 7.55), ylim = c(0.45, 5.15), clip = "off") +
  labs(
    title = "Esquema conceptual de filtros",
    subtitle = paste(
      "La representación digital de la flora altoandina depende de filtros",
      "territoriales, de detección e identificación."
    )
  ) +
  theme_void(base_family = "sans", base_size = 13) +
  theme(
    plot.title.position = "plot",
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5,
                                 margin = margin(b = 0)),
    # más aire entre título y subtítulo (t), y subtítulo más pegado al esquema (b)
    plot.subtitle = element_text(size = 11.5, hjust = 0.5,
                                 margin = margin(t = 12, b = 4)),
    plot.margin = margin(14, 16, 12, 16),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  "figures/Figura_1_esquema_conceptual_filtros.png",
  fig1, width = FIG1_WIDTH, height = FIG1_HEIGHT, dpi = 300,
  bg = "white"
)
ggsave(
  "figures/Figura_1_esquema_conceptual_filtros.pdf",
  fig1, width = FIG1_WIDTH, height = FIG1_HEIGHT,
  bg = "white"
)

# ==============================================================================
# FIGURA 2. DISEÑO 2 × 2 DE ESPECIES FOCALES
# Cada cuadrante: 3 recuadros para foto, con el nombre de la especie debajo.
# ==============================================================================

# ---- Parámetros de formato (ajustables) ---------------------------------------
FIG2_PHOTO_W     <- 0.29   # ancho del recuadro de foto (unidades de cuadrante)
FIG2_PHOTO_H     <- 0.50   # alto del recuadro de foto
SHOW_PHOTO_LABEL <- TRUE   # TRUE = escribe "foto" tenue dentro del recuadro

species_f2 <- species_master |>
  mutate(
    x0 = if_else(identifiability == "Difícil", 0, 1),
    y0 = if_else(conspicuity == "Conspicua", 1, 0)
  ) |>
  group_by(conspicuity, identifiability) |>
  arrange(species, .by_group = TRUE) |>
  mutate(
    slot     = row_number(),
    x_c      = x0 + c(1/6, 3/6, 5/6)[slot],
    ph_xmin  = x_c - FIG2_PHOTO_W / 2,
    ph_xmax  = x_c + FIG2_PHOTO_W / 2,
    ph_ymax  = y0 + 0.825,
    ph_ymin  = ph_ymax - FIG2_PHOTO_H,
    # Género / epíteto en dos líneas para que quepa bajo cada foto
    name_label = sub(" ", "\n", as.character(species))
  ) |>
  ungroup()

fig2 <- ggplot() +
  # Cuadrantes
  annotate(
    "rect",
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    fill = NA, colour = COL_LINE, linewidth = 0.8
  ) +
  annotate("segment", x = 1, xend = 1, y = 0, yend = 2,
           colour = COL_LINE, linewidth = 0.8) +
  annotate("segment", x = 0, xend = 2, y = 1, yend = 1,
           colour = COL_LINE, linewidth = 0.8) +

  # Encabezados generales
  # vjust = 1: el texto cuelga del borde superior del panel, es decir, queda
  # pegado al subtítulo (solo los separa el margen inferior del subtítulo)
  annotate("text", x = 1, y = 2.34, vjust = 1,
           label = "Identificabilidad taxonómica",
           size = 5.0) +
  annotate("text", x = 0.5, y = 2.16,
           label = "Difícil", size = 4.5) +
  annotate("text", x = 1.5, y = 2.16,
           label = "Fácil", size = 4.5) +

  annotate("text", x = -0.23, y = 1,
           label = "Conspicuidad", angle = 90, size = 5.0) +
  annotate("text", x = -0.09, y = 1.5,
           label = "Conspicua", angle = 90, size = 4.4) +
  annotate("text", x = -0.09, y = 0.5,
           label = "Inconspicua", angle = 90, size = 4.4) +

  # Recuadros para fotos
  geom_rect(
    data = species_f2,
    aes(xmin = ph_xmin, xmax = ph_xmax, ymin = ph_ymin, ymax = ph_ymax),
    fill = COL_LIGHT, colour = COL_LINE, linewidth = 0.7
  ) +
  # Rótulo tenue "foto" (se puede apagar con SHOW_PHOTO_LABEL <- FALSE)
  list(
    if (SHOW_PHOTO_LABEL) {
      geom_text(
        data = species_f2,
        aes(x = x_c, y = (ph_ymin + ph_ymax) / 2),
        label = "foto", colour = COL_SHARED, size = 4
      )
    }
  ) +

  # Nombres de especies bajo cada recuadro
  geom_text(
    data = species_f2,
    aes(x = x_c, y = ph_ymin - 0.03, label = name_label),
    hjust = 0.5, vjust = 1,
    size = 4.2, fontface = "italic", lineheight = 0.95
  ) +

  coord_cartesian(
    # xlim simétrico respecto de la matriz (centro en x = 1) para que título y
    # subtítulo queden centrados sobre los cuadrantes
    xlim = c(-0.30, 2.30),
    ylim = c(0, 2.34),
    clip = "off"
  ) +
  labs(
    title = "Diseño 2×2 de las especies focales",
    subtitle = paste(
      "Las 12 especies se distribuyeron en cuatro combinaciones balanceadas",
      "de conspicuidad e identificabilidad taxonómica."
    )
  ) +
  theme_void(base_family = "sans", base_size = 13) +
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5,
                                 margin = margin(b = 0)),
    # más aire entre título y subtítulo (t), y subtítulo más cerca de
    # "Identificabilidad taxonómica" (b)
    plot.subtitle = element_text(size = 11.5, hjust = 0.5,
                                 margin = margin(t = 12, b = 0)),
    plot.margin = margin(12, 16, 10, 16),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  "figures/Figura_2_matriz_2x2_especies.png",
  fig2, width = 11.2, height = 8.8, dpi = 300,
  bg = "white"
)
ggsave(
  "figures/Figura_2_matriz_2x2_especies.pdf",
  fig2, width = 11.2, height = 8.8,
  bg = "white"
)

# ==============================================================================
# FIGURA 3. COMPOSICIÓN DIGITAL POR ESPECIE
# Versión apaisada para poster: título centrado en toda la figura, sin subtítulo,
# barras delgadas y formato más largo que alto.
# ==============================================================================

# ---- Parámetros de formato (ajustables) ---------------------------------------
FIG3_WIDTH   <- 14.2   # ancho en pulgadas
FIG3_HEIGHT  <- 6.6    # alto en pulgadas (menor alto = barras más delgadas)
FIG3_BAR_W   <- 0.55   # grosor de las barras (0-1); antes 0.72
FIG3_STRIP_S <- 10.5   # tamaño del texto de clasificación (izquierda)
FIG3_STRIP_PAD <- 34   # margen derecho extra (pt) = ancho aprox. de la columna de
                       # clasificación de la izquierda; centra panel, rótulo y leyenda

comp_long <- species_master |>
  select(species, group, herbarium, inat, total) |>
  pivot_longer(
    cols = c(herbarium, inat),
    names_to = "source",
    values_to = "n"
  ) |>
  mutate(
    source = recode(
      source,
      herbarium = "Herbario GBIF",
      inat = "iNaturalist"
    ),
    proportion = 100 * n / total,
    species = factor(species, levels = rev(species_levels)),
    group = factor(group, levels = rev(group_levels))
  )

count_labels <- species_master |>
  mutate(
    species = factor(species, levels = rev(species_levels)),
    group = factor(group, levels = rev(group_levels)),
    label = as.character(total)
  )

# Valor de cada fuente DENTRO de su segmento (iNaturalist a la izquierda,
# herbario a la derecha, igual que el apilado de las barras). Se omiten los ceros.
seg_labels <- bind_rows(
  count_labels |>
    filter(inat > 0) |>
    transmute(species, group, x = 50 * inat / total, label = as.character(inat)),
  count_labels |>
    filter(herbarium > 0) |>
    transmute(species, group, x = 50 + 50 * inat / total, label = as.character(herbarium))
)

# Clasificación en dos líneas (p. ej. "Inconspicua" / "× Difícil") para que
# quepa en el alto de cada bloque de 3 barras.
group_labeller <- as_labeller(function(x) sub(" × ", "\n× ", x))

fig3 <- ggplot(comp_long, aes(x = proportion, y = species, fill = source)) +
  geom_col(width = FIG3_BAR_W) +
  # Valores por fuente dentro de las barras
  geom_text(
    data = seg_labels,
    aes(x = x, y = species, label = label),
    inherit.aes = FALSE,
    colour = "white", fontface = "bold", size = 3.6
  ) +
  # Total de cada barra, al final de ella
  geom_text(
    data = count_labels,
    aes(x = 101.5, y = species, label = label),
    inherit.aes = FALSE,
    hjust = 0, fontface = "bold", size = 3.9
  ) +
  facet_grid(
    rows = vars(group),
    scales = "free_y",
    space = "free_y",
    switch = "y",
    labeller = labeller(group = group_labeller)
  ) +
  scale_fill_manual(
    values = c(
      "Herbario GBIF" = COL_HERB,
      "iNaturalist" = COL_INAT
    )
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 108),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Composición digital por especie",
    x = "Composición relativa de registros (%)",
    y = NULL
  ) +
  # Eje derecho "espejo" (mismas etiquetas, invisibles) + margen derecho extra:
  # el panel queda centrado en la imagen y con él el rótulo del eje x y la leyenda.
  guides(y.sec = "axis") +
  theme_poster +
  theme(
    # Centra el título respecto de TODA la figura (no solo del panel)
    plot.title.position = "plot",
    plot.margin = margin(14, 18 + FIG3_STRIP_PAD, 14, 18),
    axis.text.y.right = element_text(colour = "transparent"),
    plot.title = element_text(
      size = 18, face = "bold", hjust = 0.5,
      margin = margin(b = 10)
    ),
    panel.grid.major.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      angle = 90,
      size = FIG3_STRIP_S,
      face = "plain",
      lineheight = 0.95,
      margin = margin(r = 6)
    ),
    axis.text.y = element_text(
      face = "italic",
      size = 11
    ),
    panel.spacing.y = unit(0.40, "lines"),
    legend.position = "bottom"
  )

ggsave(
  "figures/Figura_3_composicion_digital_especie_poster.png",
  fig3, width = FIG3_WIDTH, height = FIG3_HEIGHT, dpi = 300,
  bg = "white"
)
ggsave(
  "figures/Figura_3_composicion_digital_especie_poster.pdf",
  fig3, width = FIG3_WIDTH, height = FIG3_HEIGHT,
  bg = "white"
)

# ==============================================================================
# FIGURA 4. COMPLEMENTARIEDAD ESPACIAL
# 2000–2025 | grilla 10 × 10 km
# ==============================================================================

# ---- Parámetros de formato (ajustables) ---------------------------------------
FIG4_WIDTH  <- 12.5   # ancho en pulgadas
FIG4_HEIGHT <- 5.6    # alto en pulgadas (menor alto = barras más delgadas)
FIG4_BAR_W  <- 0.55   # grosor de las barras (0-1); antes 0.72

spatial <- tribble(
  ~species,                    ~only_herbarium, ~shared, ~only_inat,
  "Arenaria rivularis",              2,            0,        7,
  "Arenaria serpens",                2,            0,        1,
  "Calandrinia compacta",            2,            0,        8,
  "Deschampsia eminens",             6,            0,        1,
  "Gamocarpha ventosa",              1,            0,        1,
  "Oxychloe andina",                 1,            0,       10,
  "Puccinellia frigida",            17,            0,        2,
  "Triglochin concinna",             3,            0,        2,
  "Zameioscirpus atacamensis",       2,            0,        2
) |>
  mutate(total_cells = only_herbarium + shared + only_inat)

spatial_order <- spatial |>
  arrange(total_cells) |>
  pull(species)

spatial_long <- spatial |>
  pivot_longer(
    cols = c(only_herbarium, shared, only_inat),
    names_to = "coverage",
    values_to = "cells"
  ) |>
  mutate(
    coverage = recode(
      coverage,
      only_herbarium = "Solo herbario",
      shared = "Compartidas",
      only_inat = "Solo iNaturalist"
    ),
    coverage = factor(
      coverage,
      levels = c("Solo herbario", "Compartidas", "Solo iNaturalist")
    ),
    species = factor(species, levels = spatial_order)
  )

spatial_labels <- spatial |>
  mutate(species = factor(species, levels = spatial_order))

fig4 <- ggplot(spatial_long, aes(x = cells, y = species, fill = coverage)) +
  geom_col(width = FIG4_BAR_W) +
  geom_text(
    data = spatial_labels,
    aes(x = total_cells + 0.22, y = species, label = total_cells),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "Solo herbario" = COL_HERB,
      "Compartidas" = COL_SHARED,
      "Solo iNaturalist" = COL_INAT
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 5),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Complementariedad espacial entre herbarios e iNaturalist",
    x = "Número de celdas de 10 × 10 km",
    y = NULL
  ) +
  # Eje derecho "espejo" (mismas etiquetas, invisibles): hace que el panel quede
  # centrado en la imagen y, con él, el rótulo del eje x y la leyenda.
  guides(y.sec = "axis") +
  theme_poster +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5,
                              margin = margin(b = 10)),
    axis.text.y = element_text(face = "italic", size = 11),
    axis.text.y.right = element_text(colour = "transparent"),
    panel.grid.major.y = element_blank(),
    # línea del eje x que une todos los rangos (0, 5, 10, 15, 20...)
    axis.line.x  = element_line(colour = COL_TEXT, linewidth = 0.4),
    # más aire entre el gráfico y el rótulo del eje x
    axis.text.x  = element_text(margin = margin(t = 4)),
    axis.title.x = element_text(size = 13, margin = margin(t = 16)),
    legend.position = "bottom"
  )

ggsave(
  "figures/Figura_4_complementariedad_espacial.png",
  fig4, width = FIG4_WIDTH, height = FIG4_HEIGHT, dpi = 300,
  bg = "white"
)
ggsave(
  "figures/Figura_4_complementariedad_espacial.pdf",
  fig4, width = FIG4_WIDTH, height = FIG4_HEIGHT,
  bg = "white"
)

write_csv(spatial, "analysis/complementariedad_espacial_2000_2025.csv")

# ==============================================================================
# FIGURA 5. ACCESIBILIDAD TERRITORIAL
# Mediana de distancia a la red vial MOP | 2000–2025
# ==============================================================================

# ---- Parámetros de formato (ajustables) ---------------------------------------
FIG5_WIDTH  <- 14.2   # ancho en pulgadas
FIG5_HEIGHT <- 5.2    # alto en pulgadas (menor alto = figura más apaisada)
FIG5_PT     <- 3.8    # tamaño de los puntos
FIG5_LINE   <- 1.0    # grosor de la línea que une ambos puntos

accessibility <- tribble(
  ~species,                    ~n_herbarium, ~median_herb_km, ~n_inat, ~median_inat_km,
  "Arenaria rivularis",              2,           17.21,          9,        0.12,
  "Arenaria serpens",                2,            9.72,          1,       17.29,
  "Calandrinia compacta",            2,            7.85,          8,        0.83,
  "Deschampsia eminens",             6,            0.12,          1,        0.07,
  "Gamocarpha ventosa",              1,           25.16,          1,        0.01,
  "Oxychloe andina",                 2,            1.24,         11,        0.44,
  "Puccinellia frigida",            22,            0.12,          2,        2.26,
  "Triglochin concinna",             4,           12.94,          2,        2.65,
  "Zameioscirpus atacamensis",       2,            7.89,          2,        1.75
) |>
  mutate(
    diff = median_inat_km - median_herb_km
  )

access_order <- accessibility |>
  arrange(diff) |>
  pull(species)

accessibility <- accessibility |>
  mutate(species = factor(species, levels = rev(access_order)))

fig5 <- ggplot(accessibility, aes(y = species)) +
  geom_segment(
    aes(
      x = median_herb_km,
      xend = median_inat_km,
      yend = species
    ),
    linewidth = FIG5_LINE,
    colour = COL_SHARED
  ) +
  geom_point(
    aes(x = median_herb_km, colour = "Herbario GBIF"),
    size = FIG5_PT
  ) +
  geom_point(
    aes(x = median_inat_km, colour = "iNaturalist"),
    size = FIG5_PT
  ) +
  scale_colour_manual(
    values = c(
      "Herbario GBIF" = COL_HERB,
      "iNaturalist" = COL_INAT
    )
  ) +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10),
    labels = c("0,01", "0,1", "1", "10"),
    limits = c(0.007, 40)
  ) +
  # Marcas logarítmicas menores bajo el eje (como en la figura de referencia)
  annotation_logticks(
    sides = "b", outside = TRUE,
    short = unit(0.08, "cm"), mid = unit(0.14, "cm"), long = unit(0.22, "cm"),
    colour = COL_TEXT, linewidth = 0.3
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Accesibilidad territorial de los registros",
    x = "Mediana de distancia al camino más cercano (km)",
    y = NULL
  ) +
  # Eje derecho "espejo" (mismas etiquetas, invisibles): centra el panel, el
  # rótulo del eje x y la leyenda respecto de la imagen completa.
  guides(y.sec = "axis") +
  theme_poster +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5,
                              margin = margin(b = 10)),
    axis.text.y = element_text(face = "italic", size = 11),
    axis.text.y.right = element_text(colour = "transparent"),
    axis.line.x = element_line(colour = COL_TEXT, linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  "figures/Figura_5_accesibilidad_territorial.png",
  fig5, width = FIG5_WIDTH, height = FIG5_HEIGHT, dpi = 300,
  bg = "white"
)
ggsave(
  "figures/Figura_5_accesibilidad_territorial.pdf",
  fig5, width = FIG5_WIDTH, height = FIG5_HEIGHT,
  bg = "white"
)

write_csv(accessibility, "analysis/accesibilidad_territorial_2000_2025.csv")

# ==============================================================================
# 6. HOJA DE CONTACTO OPCIONAL
# ==============================================================================

contact_sheet <- (
  (fig1 / fig2) |
  (fig3 / fig4 / fig5)
) +
  plot_annotation(
    title = "Figuras 1–5 | Póster flora altoandina del norte de Chile"
  )

ggsave(
  "figures/Hoja_contacto_Figuras_1_5.pdf",
  contact_sheet,
  width = 18,
  height = 20,
  bg = "white"
)

# ==============================================================================
# FIN
# ==============================================================================
# Archivos generados:
# figures/Figura_mapa_area_estudio_chile_v11.png / .pdf
# figures/Figura_1_esquema_conceptual_filtros.png / .pdf
# figures/Figura_2_matriz_2x2_especies.png / .pdf
# figures/Figura_3_composicion_digital_especie_poster.png / .pdf
# figures/Figura_4_complementariedad_espacial.png / .pdf
# figures/Figura_5_accesibilidad_territorial.png / .pdf
# figures/Hoja_contacto_Figuras_1_5.pdf
#
# Tablas:
# analysis/species_master_norte.csv
# analysis/complementariedad_espacial_2000_2025.csv
# analysis/accesibilidad_territorial_2000_2025.csv
