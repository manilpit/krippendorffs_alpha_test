# 5_analysis.R
source("4_functions.R")

# Hent criteria columns fra funksjonen
criteria_columns <- get_criteria_columns()

# Sett opp parallel processing
cl <- setup_parallel()

# Kjør analysen
results <- run_analysis(cl, criteria_columns)

# Stopp cluster
stopCluster(cl)

# Formater resultatene
formatted_results <- format_results(results, criteria_columns)

# Vis de første resultatene
head(formatted_results)

# Test på ett enkelt kriterie for å verifisere
test_result <- test_single_criteria(test3, "criteria_1a")

# Lagre resultatene
write.csv(formatted_results, "krippendorff_results.csv", row.names = FALSE)

# Print sammendrag av resultater
summary_stats <- formatted_results %>%
  summarise(
    mean_alpha = mean(alpha, na.rm = TRUE),
    median_alpha = median(alpha, na.rm = TRUE),
    sd_alpha = sd(alpha, na.rm = TRUE),
    min_alpha = min(alpha, na.rm = TRUE),
    max_alpha = max(alpha, na.rm = TRUE)
  )

print("Summary statistics for alpha values:")
print(summary_stats)

# Identifiser kriterier med lav enighet (alpha < 0.67)
low_agreement <- formatted_results %>%
  filter(alpha < 0.67) %>%
  arrange(alpha)

print("\nCriteria with low agreement (alpha < 0.67):")
print(low_agreement)

# Identifiser kriterier med høy enighet (alpha >= 0.80)
high_agreement <- formatted_results %>%
  filter(alpha >= 0.80) %>%
  arrange(desc(alpha))

print("\nCriteria with high agreement (alpha >= 0.80):")
print(high_agreement)