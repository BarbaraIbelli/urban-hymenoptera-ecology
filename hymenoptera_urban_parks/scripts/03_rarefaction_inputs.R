# Campaign-level richness rarefaction and extrapolation (q = 0).
# Uses twice the observed number of individuals as an endpoint in EACH campaign.
# This script does not recreate the final publication figure or assume identical plotting settings.
source(file.path("scripts", "01_validate_describe.R"), encoding="UTF-8")
by_campaign <- lapply(split(seq_len(nrow(samples)), samples$campaign),
  function(i) colSums(X[i, , drop=FALSE]))
print(sapply(by_campaign, function(v) c(individuals=sum(v), superfamilies=sum(v>0))))
if (requireNamespace("iNEXT", quietly=TRUE)) {
  set.seed(2026)
  estimates <- lapply(names(by_campaign), function(campaign_name) {
    counts <- by_campaign[[campaign_name]]
    iNEXT::iNEXT(x=list(counts), q=0, datatype="abundance",
                endpoint=2*sum(counts), se=TRUE, conf=0.95, nboot=200)
  })
  names(estimates) <- names(by_campaign)
  saveRDS(estimates, "outputs/iNEXT_superfamily_richness.rds")
  cat("Wrote outputs/iNEXT_superfamily_richness.rds (separate campaign endpoints).\n")
} else {
  message("Optional package iNEXT not installed: campaign inputs validated; no curve computed.")
}
