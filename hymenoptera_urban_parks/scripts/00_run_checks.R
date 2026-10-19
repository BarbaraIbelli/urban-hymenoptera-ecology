# Execute from the GitHub repository ROOT.
# This script validates the shared dataset and computes selected ecological metrics.
# It does not reproduce every fitted model or multivariate test.

if (!file.exists('hymenoptera_urban_parks/data/hymenoptera_samples.csv')) {
  stop('Set the R working directory to the GitHub repository root.')
}
setwd('hymenoptera_urban_parks')
source('scripts/01_validate_describe.R', encoding='UTF-8')
source('scripts/02_exact_landscape_correlations.R', encoding='UTF-8')
source('scripts/03_rarefaction_inputs.R', encoding='UTF-8')
if (requireNamespace('vegan', quietly=TRUE)) {
  source('scripts/04_community_inputs.R', encoding='UTF-8')
} else {
  message('Optional vegan package not installed; community dissimilarity not computed.')
}
cat('Selected ecological checks completed. See outputs/ and README.md.\n')
