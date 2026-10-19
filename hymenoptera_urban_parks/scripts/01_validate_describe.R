# Urban Hymenoptera dataset: validation and descriptive ecological metrics
# Run from the GitHub project root (working directory is the folder containing scripts/).
options(stringsAsFactors = FALSE)
biological_file <- file.path("data", "hymenoptera_samples.csv")
landscape_file <- file.path("data", "landscape_predictors.csv")
if (!file.exists(biological_file) || !file.exists(landscape_file)) {
  stop("Both CSV files must be available in the local data/ folder.")
}
samples <- read.csv(biological_file, check.names = FALSE, stringsAsFactors = FALSE)
landscape <- read.csv(landscape_file, check.names = FALSE, stringsAsFactors = FALSE)
superfamilies <- c("Apoidea", "Chalcidoidea", "Ceraphronoidea", "Chrysidoidea",
 "Cynipoidea", "Diaprioidea", "Evanioidea", "Formicoidea", "Ichneumonoidea",
 "Platygastroidea", "Proctotrupoidea", "Tenthredinoidea", "Trigonaloidea", "Vespoidea")
stopifnot(all(c("sample_id", "park_id", "campaign", "sampling_interval", "trapping_days",
                    superfamilies) %in% names(samples)),
          nrow(samples) == 156L, nrow(landscape) == 6L,
          !anyNA(samples), !anyNA(landscape),
          !anyDuplicated(samples$sample_id),
          all(samples$trapping_days > 0),
          all(table(samples$park_id, samples$campaign) == 13L),
          all(samples$park_id %in% landscape$park_id))
X <- as.matrix(samples[, superfamilies])
storage.mode(X) <- "numeric"
stopifnot(all(is.finite(X)), all(X >= 0), all(X == floor(X)))
samples$abundance <- rowSums(X)
samples$richness <- rowSums(X > 0)
calc_shannon <- function(x) {
  if (sum(x) == 0) return(NA_real_)
  p <- x[x > 0] / sum(x)
  -sum(p * log(p))
}
samples$shannon <- apply(X, 1L, calc_shannon)
samples$simpson_1_minus_D <- apply(X, 1L, function(x) {
  p <- x[x > 0] / sum(x)
  1 - sum(p^2)
})
samples$pielou <- ifelse(samples$richness > 1L,
                         samples$shannon / log(samples$richness), NA_real_)
stopifnot(sum(samples$abundance) == 62147L,
          sum(samples$abundance[samples$campaign == "2023-2024"]) == 32515L,
          sum(samples$abundance[samples$campaign == "2024-2025"]) == 29632L,
          sum(samples$trapping_days) == 1666L)
park_expected <- c(P1=3822L, P2=6745L, P3=25605L, P4=4066L, P5=10618L, P6=11291L)
park_actual <- tapply(samples$abundance, samples$park_id, sum)
stopifnot(identical(as.numeric(park_actual[names(park_expected)]), as.numeric(park_expected)))

park_metrics <- do.call(rbind, lapply(split(samples, samples$park_id), function(x) {
  data.frame(park_id=x$park_id[1], capture_rate=sum(x$abundance)/sum(x$trapping_days),
             mean_richness=mean(x$richness), mean_shannon=mean(x$shannon))
}))
rownames(park_metrics) <- NULL
park_metrics <- merge(park_metrics, landscape, by="park_id", sort=TRUE)
if (!dir.exists("outputs")) dir.create("outputs")
write.csv(park_metrics, "outputs/park_metrics_reconstructed.csv", row.names=FALSE)
write.csv(samples[, c("sample_id","park_id","campaign","sampling_interval","trapping_days",
                      "abundance","richness","shannon","simpson_1_minus_D","pielou")],
          "outputs/sample_metrics_reconstructed.csv", row.names=FALSE)
cat("Validated 156 samples, 14 superfamilies, 62,147 individuals, 1,666 effective trap days.\n")
print(park_metrics[, c("park_id","capture_rate","mean_richness","mean_shannon")])
