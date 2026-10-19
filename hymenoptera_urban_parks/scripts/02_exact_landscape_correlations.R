# Exact permutation p-values for the 33 park-level Spearman tests.
# Uses six park-level observations; BH correction within each response (11 tests).
source(file.path("scripts", "01_validate_describe.R"), encoding="UTF-8")
permutations <- function(x) {
  if (length(x) == 1L) return(matrix(x, nrow=1L))
  do.call(rbind, lapply(seq_along(x), function(i) {
    cbind(x[i], permutations(x[-i]))
  }))
}
idx <- permutations(seq_len(6L))  # all 6! = 720 equally weighted label permutations
stopifnot(nrow(idx) == 720L)
features <- c("fragment_area_ha", "enn_m",
             "ndvi_250m", "ndvi_500m", "ndvi_1000m",
             "natural_vegetation_pct_250m", "natural_vegetation_pct_500m",
             "natural_vegetation_pct_1000m", "urbanized_area_pct_250m",
             "urbanized_area_pct_500m", "urbanized_area_pct_1000m")
responses <- c("capture_rate", "mean_richness", "mean_shannon")
stopifnot(all(features %in% names(park_metrics)), all(responses %in% names(park_metrics)))
exact_test <- function(x, y) {
  obs <- suppressWarnings(cor(x,y,method="spearman"))
  null_values <- apply(idx, 1L, function(ind) suppressWarnings(cor(x,y[ind],method="spearman")))
  tolerance <- 1e-10
  c(rho=obs, p_exact=sum(abs(null_values) >= (abs(obs)-tolerance))/length(null_values))
}
results <- do.call(rbind, lapply(responses, function(res) {
  out <- do.call(rbind,lapply(features, function(feature) {
    ans <- exact_test(park_metrics[[res]], park_metrics[[feature]])
    data.frame(response=res, predictor=feature, rho=unname(ans["rho"]),
               p_exact=unname(ans["p_exact"]))
  }))
  out$p_bh <- p.adjust(out$p_exact, method="BH")
  out
}))
rownames(results) <- NULL
write.csv(results, "outputs/landscape_correlations_exact.csv", row.names=FALSE)
cat("Reconstructed 33 exact (720-permutation) park-level Spearman tests.\n")
print(results[results$response=="mean_shannon" &
                  results$predictor %in% c("enn_m", "natural_vegetation_pct_500m"), ])
