# =============================================================================
# Figure 8 — Landscape correlates of Hymenoptera assemblages across spatial scales
# English version for manuscript submission
#
# Reproducible script for the public repository.
#
# Inputs:
#   resultados/tabelas/base_espacial_3_5.csv
#   resultados/tabelas/correlacoes_paisagem_3_5.csv
#
# Outputs:
#   resultados/figuras/Figure_8_landscape_correlates_EN.png
#   resultados/figuras/Figure_8_landscape_correlates_EN.pdf
#   resultados/figuras/Figure_8_landscape_correlates_EN_600dpi.tiff
#   resultados/figuras/Figure_8_landscape_correlates_EN_sessionInfo.txt
#
# Run from the project root (analises_tese_3_5.Rproj).
# No stochastic procedure is used; therefore, no random seed is required.
# =============================================================================


# 1. Packages -----------------------------------------------------------------

pkgs <- c(
  "readr",
  "dplyr",
  "ggplot2",
  "patchwork",
  "scales",
  "here"
)

missing_pkgs <- pkgs[
  !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0L) {
  stop(
    "Install required package(s): ",
    paste(missing_pkgs, collapse = ", ")
  )
}

options(OutDec = ".")


# 2. Paths --------------------------------------------------------------------

input_dir  <- here::here("resultados", "tabelas")
output_dir <- here::here("resultados", "figuras")

base_file <- file.path(input_dir, "base_espacial_3_5.csv")
corr_file <- file.path(input_dir, "correlacoes_paisagem_3_5.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(base_file)) {
  stop("Input file not found: ", base_file)
}

if (!file.exists(corr_file)) {
  stop("Input file not found: ", corr_file)
}


# 3. Import -------------------------------------------------------------------

base <- readr::read_csv(
  base_file,
  show_col_types = FALSE,
  progress = FALSE
)

corr <- readr::read_csv(
  corr_file,
  show_col_types = FALSE,
  progress = FALSE
)


# 4. Validate inputs -----------------------------------------------------------

required_base_cols <- c(
  "Parque",
  "ENN_m",
  "Shannon_medio",
  "NDVI_250",
  "NDVI_500",
  "NDVI_1000",
  "Vegetacao_natural_250_prop",
  "Vegetacao_natural_500_prop",
  "Vegetacao_natural_1000_prop",
  "Urbano_250_prop",
  "Urbano_500_prop",
  "Urbano_1000_prop"
)

required_corr_cols <- c(
  "Resposta",
  "Preditor",
  "Escala_m",
  "rho",
  "p",
  "p_BH"
)

missing_base_cols <- setdiff(required_base_cols, names(base))
missing_corr_cols <- setdiff(required_corr_cols, names(corr))

if (length(missing_base_cols) > 0L) {
  stop(
    "Missing column(s) in base_espacial_3_5.csv: ",
    paste(missing_base_cols, collapse = ", ")
  )
}

if (length(missing_corr_cols) > 0L) {
  stop(
    "Missing column(s) in correlacoes_paisagem_3_5.csv: ",
    paste(missing_corr_cols, collapse = ", ")
  )
}

park_order <- paste0("P", 1:6)

if (nrow(base) != 6L) {
  stop("The spatial database must contain exactly six parks.")
}

if (!setequal(base$Parque, park_order)) {
  stop("Park identifiers must be P1, P2, P3, P4, P5, and P6.")
}

if (anyDuplicated(base$Parque) > 0L) {
  stop("Duplicate park identifiers detected.")
}

if (anyNA(base[, required_base_cols])) {
  stop("Missing values detected in variables required for Figure 8.")
}

base <- base |>
  dplyr::mutate(
    Parque = factor(.data$Parque, levels = park_order)
  ) |>
  dplyr::arrange(.data$Parque)


# 5. Validate plotted Spearman coefficients -----------------------------------

response_shannon <- "Shannon médio"
predictor_nat     <- "Vegetação natural"
predictor_urban   <- "Área urbanizada"

rho_from_file <- function(predictor, scale = NA_real_) {
  
  out <- corr |>
    dplyr::filter(
      .data$Resposta == response_shannon,
      .data$Preditor == predictor
    )
  
  if (is.na(scale)) {
    out <- out |>
      dplyr::filter(is.na(.data$Escala_m))
  } else {
    out <- out |>
      dplyr::filter(.data$Escala_m == scale)
  }
  
  if (nrow(out) != 1L) {
    stop(
      "Expected exactly one validated correlation for predictor '",
      predictor,
      "' and scale '",
      ifelse(is.na(scale), "not applicable", scale),
      "'."
    )
  }
  
  out$rho[[1]]
}

spearman_rho <- function(x, y) {
  unname(
    stats::cor(
      x,
      y,
      method = "spearman",
      use = "complete.obs"
    )
  )
}

validation <- data.frame(
  predictor = c(
    "ENN",
    predictor_nat, predictor_nat, predictor_nat,
    "NDVI", "NDVI", "NDVI",
    predictor_urban, predictor_urban, predictor_urban
  ),
  scale = c(
    NA,
    250, 500, 1000,
    250, 500, 1000,
    250, 500, 1000
  ),
  rho_recalculated = c(
    spearman_rho(base$ENN_m, base$Shannon_medio),
    spearman_rho(base$Vegetacao_natural_250_prop, base$Shannon_medio),
    spearman_rho(base$Vegetacao_natural_500_prop, base$Shannon_medio),
    spearman_rho(base$Vegetacao_natural_1000_prop, base$Shannon_medio),
    spearman_rho(base$NDVI_250, base$Shannon_medio),
    spearman_rho(base$NDVI_500, base$Shannon_medio),
    spearman_rho(base$NDVI_1000, base$Shannon_medio),
    spearman_rho(base$Urbano_250_prop, base$Shannon_medio),
    spearman_rho(base$Urbano_500_prop, base$Shannon_medio),
    spearman_rho(base$Urbano_1000_prop, base$Shannon_medio)
  )
)

validation$rho_validated <- mapply(
  rho_from_file,
  validation$predictor,
  validation$scale
)

if (
  any(
    abs(
      validation$rho_recalculated -
      validation$rho_validated
    ) > 1e-8
  )
) {
  stop(
    "Recalculated Spearman coefficients do not match ",
    "the validated correlation table."
  )
}

rho_enn    <- rho_from_file("ENN")
rho_nat500 <- rho_from_file(predictor_nat, 500)


# 6. Prepare data --------------------------------------------------------------

dat_ab <- base |>
  dplyr::transmute(
    Parque,
    ENN = .data$ENN_m,
    Shannon = .data$Shannon_medio,
    Natural500 = 100 * .data$Vegetacao_natural_500_prop
  )

# Fixed positions reproduce the layout of the original figure.
# These coordinates affect labels only; analytical values are unchanged.

labels_a <- data.frame(
  Parque = factor(park_order, levels = park_order),
  label_x = c(137.5, 181.0, 57.5, 57.5, 57.5, 57.5),
  label_y = c(1.568, 1.557, 1.714, 1.618, 1.858, 1.652)
)

labels_b <- data.frame(
  Parque = factor(park_order, levels = park_order),
  label_x = c(17.0, 7.5, 15.0, 49.0, 94.0, 78.0),
  label_y = c(1.569, 1.542, 1.683, 1.651, 1.858, 1.653)
)

dat_a <- dat_ab |>
  dplyr::left_join(labels_a, by = "Parque")

dat_b <- dat_ab |>
  dplyr::left_join(labels_b, by = "Parque")

dat_c <- corr |>
  dplyr::filter(
    .data$Resposta == response_shannon,
    .data$Preditor %in% c(
      "NDVI",
      predictor_nat,
      predictor_urban
    ),
    .data$Escala_m %in% c(250, 500, 1000)
  ) |>
  dplyr::mutate(
    Predictor_EN = dplyr::recode(
      .data$Preditor,
      "NDVI" = "NDVI",
      "Vegetação natural" = "Natural vegetation",
      "Área urbanizada" = "Urbanized area"
    ),
    Predictor_EN = factor(
      .data$Predictor_EN,
      levels = c(
        "NDVI",
        "Natural vegetation",
        "Urbanized area"
      )
    )
  ) |>
  dplyr::arrange(
    .data$Predictor_EN,
    .data$Escala_m
  )

if (nrow(dat_c) != 9L) {
  stop("Panel C must contain exactly nine coefficients.")
}


# 7. Visual settings -----------------------------------------------------------

park_colors <- c(
  "P1" = "#440154",
  "P2" = "#414487",
  "P3" = "#2A788E",
  "P4" = "#22A884",
  "P5" = "#7AD151",
  "P6" = "#FDE725"
)

landscape_colors <- c(
  "NDVI" = "#5B5B5B",
  "Natural vegetation" = "#177D3C",
  "Urbanized area" = "#C51B2D"
)

theme_article <- ggplot2::theme_classic(
  base_size = 10.5,
  base_family = "sans"
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 11.8,
      hjust = 0,
      margin = ggplot2::margin(b = 5)
    ),
    axis.title = ggplot2::element_text(
      size = 10.7
    ),
    axis.text = ggplot2::element_text(
      size = 9.8,
      colour = "black"
    ),
    axis.line = ggplot2::element_line(
      linewidth = 0.65,
      colour = "black"
    ),
    axis.ticks = ggplot2::element_line(
      linewidth = 0.55,
      colour = "black"
    ),
    plot.margin = ggplot2::margin(
      t = 5,
      r = 8,
      b = 4,
      l = 5
    )
  )


# 8. Panel A — Structural isolation -------------------------------------------

p_a <- ggplot2::ggplot(
  dat_a,
  ggplot2::aes(
    x = .data$ENN,
    y = .data$Shannon
  )
) +
  ggplot2::geom_point(
    ggplot2::aes(
      colour = .data$Parque
    ),
    size = 3.25
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = .data$label_x,
      y = .data$label_y,
      label = .data$Parque,
      colour = .data$Parque
    ),
    size = 3.15,
    show.legend = FALSE
  ) +
  # Using x = Inf with right justification keeps the complete annotation
  # inside the plotting panel regardless of device dimensions.
  ggplot2::annotate(
    "text",
    x = Inf,
    y = Inf,
    label = sprintf("\u03c1 = %.3f", rho_enn),
    hjust = 1.08,
    vjust = 1.35,
    size = 3.45
  ) +
  ggplot2::scale_colour_manual(
    values = park_colors,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(100, 150),
    limits = c(45, 194),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(1.6, 1.7, 1.8),
    limits = c(1.525, 1.895),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::labs(
    title = "(a) Structural isolation",
    x = "ENN (m)",
    y = "Mean Shannon diversity"
  ) +
  theme_article


# 9. Panel B — Natural vegetation at 500 m ------------------------------------

p_b <- ggplot2::ggplot(
  dat_b,
  ggplot2::aes(
    x = .data$Natural500,
    y = .data$Shannon
  )
) +
  ggplot2::geom_point(
    ggplot2::aes(
      colour = .data$Parque
    ),
    size = 3.25
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = .data$label_x,
      y = .data$label_y,
      label = .data$Parque,
      colour = .data$Parque
    ),
    size = 3.15,
    show.legend = FALSE
  ) +
  # Positioned left of P5 to reproduce the original figure and avoid overlap.
  ggplot2::annotate(
    "text",
    x = 66,
    y = 1.878,
    label = sprintf("\u03c1 = %.3f", rho_nat500),
    hjust = 0,
    size = 3.45
  ) +
  ggplot2::scale_colour_manual(
    values = park_colors,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    labels = scales::label_percent(
      scale = 1,
      accuracy = 1
    ),
    limits = c(-5, 104),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(1.6, 1.7, 1.8),
    limits = c(1.525, 1.895),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::labs(
    title = "(b) Natural vegetation \u2014 500 m",
    x = "Natural vegetation in buffer (%)",
    y = "Mean Shannon diversity"
  ) +
  theme_article


# 10. Panel C — Multiscale pattern --------------------------------------------

p_c <- ggplot2::ggplot(
  dat_c,
  ggplot2::aes(
    x = .data$Escala_m,
    y = .data$rho,
    colour = .data$Predictor_EN,
    group = .data$Predictor_EN
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey65"
  ) +
  ggplot2::geom_line(
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    size = 3.25
  ) +
  ggplot2::scale_colour_manual(
    values = landscape_colors
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(250, 500, 1000),
    labels = c("250", "500", "1,000"),
    limits = c(100, 1150),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(-1, 1, by = 0.5),
    limits = c(-1.08, 1.10),
    labels = scales::label_number(
      accuracy = 0.1
    ),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::labs(
    title = "(c) Multiscale pattern",
    x = "Spatial scale (m)",
    y = "Spearman \u03c1",
    colour = NULL
  ) +
  theme_article +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = ggplot2::element_text(
      size = 9.5
    ),
    legend.key.width = grid::unit(
      1.05,
      "cm"
    ),
    legend.spacing.x = grid::unit(
      0.10,
      "cm"
    ),
    legend.margin = ggplot2::margin(
      t = 3,
      b = 0
    )
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 0.9,
        size = 3.0
      )
    )
  )


# 11. Assemble figure ----------------------------------------------------------

figure_8 <- (p_a + p_b) / p_c +
  patchwork::plot_layout(
    heights = c(1.00, 0.80)
  )


# 12. Export -------------------------------------------------------------------

figure_width_mm  <- 165
figure_height_mm <- 156

png_file <- file.path(
  output_dir,
  "Figure_8_landscape_correlates_EN_FINAL.png"
)

pdf_file <- file.path(
  output_dir,
  "Figure_8_landscape_correlates_EN_FINAL.pdf"
)

tiff_file <- file.path(
  output_dir,
  "Figure_8_landscape_correlates_EN_FINAL_600dpi.tiff"
)

session_file <- file.path(
  output_dir,
  "Figure_8_landscape_correlates_EN_FINAL_sessionInfo.txt"
)

ggplot2::ggsave(
  filename = png_file,
  plot = figure_8,
  width = figure_width_mm,
  height = figure_height_mm,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = figure_8,
  width = figure_width_mm,
  height = figure_height_mm,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggplot2::ggsave(
  filename = tiff_file,
  plot = figure_8,
  width = figure_width_mm,
  height = figure_height_mm,
  units = "mm",
  dpi = 600,
  device = "tiff",
  compression = "lzw",
  bg = "white"
)

utils::capture.output(
  sessionInfo(),
  file = session_file
)


# 13. Final check --------------------------------------------------------------

expected_outputs <- c(
  png_file,
  pdf_file,
  tiff_file,
  session_file
)

if (!all(file.exists(expected_outputs))) {
  stop(
    "One or more expected Figure 8 output files were not created."
  )
}

cat("\n")
cat("============================================================\n")
cat("FIGURE 8 GENERATED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Correlation validation: OK\n")
cat(
  "ENN x Shannon rho = ",
  sprintf("%.3f", rho_enn),
  "\n",
  sep = ""
)
cat(
  "Natural vegetation 500 m x Shannon rho = ",
  sprintf("%.3f", rho_nat500),
  "\n",
  sep = ""
)
cat(
  "Outputs saved in: ",
  output_dir,
  "\n",
  sep = ""
)
cat("============================================================\n")