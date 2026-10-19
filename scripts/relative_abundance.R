# =============================================================================
# Figure 5 — Relative abundance of Hymenoptera superfamilies across urban parks
# and sampling campaigns
#
# FINAL manuscript version:
# - stacked bars;
# - one fixed color per superfamily;
# - fixed segment order in every bar;
# - order defined objectively by TOTAL abundance in the complete dataset;
# - most abundant superfamilies at the bottom;
# - rarest superfamilies at the top;
# - same order in the legend;
# - validated final dataset only.
#
# Preferred input for the public repository:
#   resultados/tabelas/Figure_5_relative_abundance.csv
#
# If that standardized copy is not yet present, the script searches:
#   C:/Users/babii/OneDrive/Documents/Resultados tese
# for the validated final Figure 5 dataset.
#
# Outputs:
#   resultados/figuras/Figure_5_relative_abundance_EN_FINAL.png
#   resultados/figuras/Figure_5_relative_abundance_EN_FINAL.pdf
#   resultados/figuras/Figure_5_relative_abundance_EN_FINAL_600dpi.tiff
#   resultados/figuras/Figure_5_relative_abundance_EN_FINAL_sessionInfo.txt
#
# No stochastic procedure is used; therefore, no random seed is required.
# =============================================================================


# 1. Packages -----------------------------------------------------------------

pkgs <- c(
  "readr",
  "dplyr",
  "ggplot2",
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


# 2. Paths and validated-dataset signature ------------------------------------

output_dir <- here::here("resultados", "figuras")
input_std_dir <- here::here("resultados", "tabelas")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(input_std_dir, recursive = TRUE, showWarnings = FALSE)

preferred_input <- file.path(
  input_std_dir,
  "Figure_5_relative_abundance.csv"
)

thesis_results_dir <- "C:/Users/babii/OneDrive/Documents/Resultados tese"

required_cols <- c(
  "Parque_ID",
  "Campanha_ID",
  "Superfamilia",
  "Abundancia",
  "Abundancia_relativa",
  "Parque"
)

expected_total_abundance <- 62147L
expected_n_rows <- 168L
expected_parks <- paste0("P", 1:6)
expected_campaigns <- c("2023-2024", "2024-2025")

expected_park_totals <- c(
  P1 = 3822L,
  P2 = 6745L,
  P3 = 25605L,
  P4 = 4066L,
  P5 = 10618L,
  P6 = 11291L
)

expected_campaign_totals <- c(
  "2023-2024" = 32515L,
  "2024-2025" = 29632L
)

expected_superfamily_totals <- c(
  Formicoidea = 23185L,
  Apoidea = 8887L,
  Vespoidea = 8282L,
  Ichneumonoidea = 7751L,
  Chalcidoidea = 5450L,
  Chrysidoidea = 3197L,
  Platygastroidea = 1431L,
  Tenthredinoidea = 1144L,
  Cynipoidea = 938L,
  Diaprioidea = 712L,
  Evanioidea = 604L,
  Proctotrupoidea = 311L,
  Ceraphronoidea = 251L,
  Trigonaloidea = 4L
)

read_candidate <- function(path) {
  tryCatch(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE
    ),
    error = function(e) NULL
  )
}

matches_final_figure5_data <- function(path) {
  
  x <- read_candidate(path)
  
  if (is.null(x)) return(FALSE)
  if (!all(required_cols %in% names(x))) return(FALSE)
  if (nrow(x) != expected_n_rows) return(FALSE)
  if (anyNA(x[, required_cols])) return(FALSE)
  
  if (!setequal(unique(x$Parque), expected_parks)) return(FALSE)
  if (!setequal(unique(x$Campanha_ID), expected_campaigns)) return(FALSE)
  if (length(unique(x$Superfamilia)) != 14L) return(FALSE)
  
  if (sum(x$Abundancia) != expected_total_abundance) return(FALSE)
  
  park_totals <- tapply(x$Abundancia, x$Parque, sum)
  park_totals <- park_totals[names(expected_park_totals)]
  
  if (
    !identical(
      as.integer(park_totals),
      as.integer(expected_park_totals)
    )
  ) return(FALSE)
  
  campaign_totals <- tapply(
    x$Abundancia,
    x$Campanha_ID,
    sum
  )
  campaign_totals <- campaign_totals[names(expected_campaign_totals)]
  
  if (
    !identical(
      as.integer(campaign_totals),
      as.integer(expected_campaign_totals)
    )
  ) return(FALSE)
  
  superfamily_totals <- tapply(
    x$Abundancia,
    x$Superfamilia,
    sum
  )
  superfamily_totals <- superfamily_totals[
    names(expected_superfamily_totals)
  ]
  
  if (
    any(is.na(superfamily_totals)) ||
    !identical(
      as.integer(superfamily_totals),
      as.integer(expected_superfamily_totals)
    )
  ) return(FALSE)
  
  TRUE
}


# 3. Locate final input automatically -----------------------------------------

if (
  file.exists(preferred_input) &&
  matches_final_figure5_data(preferred_input)
) {
  
  input_file <- preferred_input
  
} else {
  
  if (!dir.exists(thesis_results_dir)) {
    stop(
      "The folder was not found: ",
      thesis_results_dir
    )
  }
  
  csv_candidates <- list.files(
    thesis_results_dir,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(csv_candidates) == 0L) {
    stop(
      "No CSV files were found in: ",
      thesis_results_dir
    )
  }
  
  exact_candidates <- csv_candidates[
    vapply(
      csv_candidates,
      matches_final_figure5_data,
      logical(1)
    )
  ]
  
  if (length(exact_candidates) == 0L) {
    stop(
      "No CSV in 'Resultados tese' matched the FINAL validated Figure 5 dataset.\n",
      "Required signature: 168 rows; 62,147 individuals; six parks; ",
      "two campaigns; 14 superfamilies; and validated totals by park, ",
      "campaign, and superfamily."
    )
  }
  
  candidate_info <- file.info(exact_candidates)
  
  exact_candidates <- exact_candidates[
    order(candidate_info$mtime, decreasing = TRUE)
  ]
  
  input_file <- exact_candidates[[1]]
  
  message(
    "Validated final Figure 5 dataset found automatically:\n",
    input_file
  )
}


# 4. Import -------------------------------------------------------------------

dat_raw <- readr::read_csv(
  input_file,
  show_col_types = FALSE,
  progress = FALSE
)

# Save a standardized project copy for future reproducible runs.
if (
  normalizePath(input_file, winslash = "/", mustWork = TRUE) !=
  normalizePath(preferred_input, winslash = "/", mustWork = FALSE)
) {
  
  readr::write_csv(
    dat_raw,
    preferred_input
  )
  
  message(
    "Standardized project copy saved to:\n",
    preferred_input
  )
}


# 5. Recalculate and validate relative abundance ------------------------------

if (!all(required_cols %in% names(dat_raw))) {
  stop(
    "Missing required column(s): ",
    paste(
      setdiff(required_cols, names(dat_raw)),
      collapse = ", "
    )
  )
}

dat <- dat_raw |>
  dplyr::group_by(
    .data$Campanha_ID,
    .data$Parque
  ) |>
  dplyr::mutate(
    total_abundance = sum(.data$Abundancia),
    relative_abundance = .data$Abundancia / .data$total_abundance
  ) |>
  dplyr::ungroup()

max_diff <- max(
  abs(
    dat$relative_abundance -
      dat$Abundancia_relativa
  )
)

if (max_diff > 1e-8) {
  warning(
    "Provided relative abundance differs from recalculated values. ",
    "The figure will use values recalculated directly from 'Abundancia'. ",
    "Maximum absolute difference = ",
    signif(max_diff, 6),
    "."
  )
}

bar_check <- dat |>
  dplyr::group_by(
    .data$Campanha_ID,
    .data$Parque
  ) |>
  dplyr::summarise(
    sum_relative = sum(.data$relative_abundance),
    .groups = "drop"
  )

if (any(abs(bar_check$sum_relative - 1) > 1e-8)) {
  stop(
    "Relative abundance does not sum to 1 for one or more ",
    "Park x Campaign combinations."
  )
}


# 6. Objective fixed order -----------------------------------------------------

# Scientific display rule:
# the same taxonomic order is retained in every bar so that a given
# superfamily has the same vertical reference across parks and campaigns.
# Order is determined objectively by total abundance in the complete dataset.

superfamily_order <- dat |>
  dplyr::group_by(.data$Superfamilia) |>
  dplyr::summarise(
    total_abundance = sum(.data$Abundancia),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(.data$total_abundance)
  ) |>
  dplyr::pull(.data$Superfamilia)

expected_order <- names(expected_superfamily_totals)

if (!identical(superfamily_order, expected_order)) {
  stop(
    "Calculated superfamily order does not match the validated final totals."
  )
}

park_order <- paste0("P", 1:6)
campaign_order <- c("2023-2024", "2024-2025")
campaign_labels <- c(
  "2023-2024" = "2023\u20132024",
  "2024-2025" = "2024\u20132025"
)

# ggplot2 stacks factor levels in reverse order by default.
# Reversing the factor levels here places Formicoidea at the bottom while
# keeping the legend in descending total-abundance order.
dat_plot <- dat |>
  dplyr::mutate(
    Parque = factor(
      .data$Parque,
      levels = park_order
    ),
    Campanha_ID = factor(
      .data$Campanha_ID,
      levels = campaign_order,
      labels = unname(campaign_labels[campaign_order])
    ),
    Superfamilia = factor(
      .data$Superfamilia,
      levels = rev(superfamily_order)
    )
  )


# 7. Final qualitative palette -------------------------------------------------

# One fixed, high-contrast color per superfamily.
# The palette deliberately distributes the dominant groups across different
# hue families instead of using a sequential gradient. Rarer taxa also receive
# distinct colors so they remain visible in thin stacked segments.

palette_superfamily <- c(
  "Formicoidea"      = "#D9E021",  # yellow-green
  "Apoidea"          = "#F28E2B",  # orange
  "Vespoidea"        = "#E6AB02",  # golden
  "Ichneumonoidea"   = "#009E73",  # teal green
  "Chalcidoidea"     = "#56B4E9",  # light blue
  "Chrysidoidea"     = "#0072B2",  # blue
  "Platygastroidea"  = "#5E9C19",  # olive green
  "Tenthredinoidea"  = "#B8DE29",  # lime
  "Cynipoidea"       = "#8DA0CB",  # periwinkle
  "Diaprioidea"      = "#1B9E77",  # green-teal
  "Evanioidea"       = "#6A51A3",  # purple
  "Proctotrupoidea"  = "#D9B98C",  # sand
  "Ceraphronoidea"   = "#E78AC3",  # pink
  "Trigonaloidea"    = "#542788"   # dark violet
)

if (!setequal(names(palette_superfamily), superfamily_order)) {
  stop(
    "Palette names do not match the validated superfamily set."
  )
}


# 8. Plot ---------------------------------------------------------------------

theme_article <- ggplot2::theme_classic(
  base_size = 10.2,
  base_family = "sans"
) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.50
    ),
    strip.text = ggplot2::element_text(
      size = 9.8,
      face = "plain"
    ),
    axis.title = ggplot2::element_text(
      size = 10.4
    ),
    axis.text = ggplot2::element_text(
      size = 9.2,
      colour = "black"
    ),
    axis.line = ggplot2::element_line(
      linewidth = 0.60,
      colour = "black"
    ),
    axis.ticks = ggplot2::element_line(
      linewidth = 0.50,
      colour = "black"
    ),
    panel.grid = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(
      size = 9.7
    ),
    legend.text = ggplot2::element_text(
      size = 8.0
    ),
    legend.key.height = grid::unit(
      0.37,
      "cm"
    ),
    legend.key.width = grid::unit(
      0.37,
      "cm"
    ),
    legend.spacing.y = grid::unit(
      0.02,
      "cm"
    ),
    legend.position = "right",
    plot.margin = ggplot2::margin(
      t = 5,
      r = 6,
      b = 4,
      l = 5
    )
  )

figure_5 <- ggplot2::ggplot(
  dat_plot,
  ggplot2::aes(
    x = .data$Parque,
    y = .data$relative_abundance,
    fill = .data$Superfamilia
  )
) +
  ggplot2::geom_col(
    width = 0.83,
    colour = "white",
    linewidth = 0.14
  ) +
  ggplot2::facet_wrap(
    ~ Campanha_ID,
    nrow = 1
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = scales::label_percent(
      accuracy = 1
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0)
    )
  ) +
  ggplot2::scale_fill_manual(
    values = palette_superfamily,
    breaks = superfamily_order,
    drop = FALSE,
    name = "Superfamily"
  ) +
  ggplot2::labs(
    x = "Park",
    y = "Relative abundance"
  ) +
  theme_article


# 9. Export -------------------------------------------------------------------

# Final two-column manuscript width.
figure_width_mm <- 165
figure_height_mm <- 92

png_file <- file.path(
  output_dir,
  "Figure_5_relative_abundance_EN_FINAL.png"
)

pdf_file <- file.path(
  output_dir,
  "Figure_5_relative_abundance_EN_FINAL.pdf"
)

tiff_file <- file.path(
  output_dir,
  "Figure_5_relative_abundance_EN_FINAL_600dpi.tiff"
)

session_file <- file.path(
  output_dir,
  "Figure_5_relative_abundance_EN_FINAL_sessionInfo.txt"
)

ggplot2::ggsave(
  filename = png_file,
  plot = figure_5,
  width = figure_width_mm,
  height = figure_height_mm,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_file,
  plot = figure_5,
  width = figure_width_mm,
  height = figure_height_mm,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggplot2::ggsave(
  filename = tiff_file,
  plot = figure_5,
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


# 10. Final validation ---------------------------------------------------------

expected_outputs <- c(
  png_file,
  pdf_file,
  tiff_file,
  session_file
)

if (!all(file.exists(expected_outputs))) {
  stop(
    "One or more expected Figure 5 output files were not created."
  )
}

cat("\n")
cat("============================================================\n")
cat("FIGURE 5 FINAL GENERATED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Input file used: ", input_file, "\n", sep = "")
cat("Number of rows: ", nrow(dat_raw), "\n", sep = "")
cat("Total abundance: ", sum(dat_raw$Abundancia), "\n", sep = "")
cat("Final dataset signature validation: OK\n")
cat(
  "Maximum absolute difference between provided and recalculated ",
  "relative abundance: ",
  signif(max_diff, 6),
  "\n",
  sep = ""
)
cat("Fixed stack and legend order (highest to lowest total abundance):\n")
cat(
  paste(superfamily_order, collapse = " > "),
  "\n"
)
cat("Formicoidea is fixed at the bottom of every bar: YES\n")
cat("Relative abundance sums to 100% in all 12 bars: YES\n")
cat("Outputs saved in: ", output_dir, "\n", sep = "")
cat("============================================================\n")
