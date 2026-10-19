# Bray-Curtis community distance input, using square-root transformed counts.
# NMDS coordinates, restricted PERMANOVA and PERMDISP are not claimed to be
# reproduced until the exact original permutation-design scripts are deposited.
source(file.path("scripts", "01_validate_describe.R"), encoding="UTF-8")
if (!requireNamespace("vegan", quietly=TRUE)) {
  stop("Install the vegan package to calculate Bray-Curtis dissimilarities.")
}
ord <- order(samples$park_id, samples$campaign, samples$sampling_interval)
community_matrix <- sqrt(X[ord, , drop=FALSE])
rownames(community_matrix) <- samples$sample_id[ord]
bray <- vegan::vegdist(community_matrix, method="bray")
saveRDS(bray, "outputs/bray_curtis_sqrt_156.rds")
write.csv(samples[ord,c("sample_id","park_id","campaign","sampling_interval")],
          "outputs/community_sample_order.csv",row.names=FALSE)
cat("Saved Bray-Curtis dissimilarity data for 156 samples; inference not rerun.\n")
