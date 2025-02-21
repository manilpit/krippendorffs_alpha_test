# 4_functions.R
source("3_data_cleaning.R")

# Process criteria funksjon
process_criteria <- function(df, criteria) {
  # Prealloker kolonner for raskere seleksjon
  required_cols <- c("kandidatnr", "rater", criteria)
  
  # Mer effektiv dataseleksjon
  df_subset <- df[, required_cols]
  
  # Raskere pivot_wider med mindre overhead
  df_wide <- tidyr::pivot_wider(
    df_subset,
    names_from = rater,
    values_from = all_of(criteria),
    values_fill = NA
  )
  
  # Direkte matrise-konvertering
  df_matrix <- as.matrix(df_wide[, -1])
  
  # Parallel processing for begge alpha-beregningene
  fit_nom <- krippendorffs.alpha(
    df_matrix,
    level = "nominal",
    confint = TRUE,
    verbose = FALSE,
    control = list(
      nodes = 2,  # Fast verdi som sikrer minimum 2 noder
      parallel = TRUE
    )
  )
  
  fit_nom_customary <- krippendorffs.alpha(
    df_matrix,
    level = "nominal",
    method = "customary",
    confint = TRUE,
    verbose = FALSE,
    control = list(
      bootit = 10000,
      nodes = 2,  # Fast verdi som sikrer minimum 2 noder
      type = "PSOCK",
      parallel = TRUE
    )
  )
  
  # Beregn konfidensintervaller
  confint_nom <- confint(fit_nom, level = 0.95)
  confint_nom_customary <- confint(fit_nom_customary, level = 0.95)
  
  return(list(
    alpha = fit_nom,
    confint_default = confint_nom,
    confint_customary = confint_nom_customary
  ))
}

# Liste over kriterier
get_criteria_columns <- function() {
  c(
    "criteria_1a", "criteria_1b", "criteria_1c", "criteria_1d", "criteria_1e", "criteria_1f",
    "criteria_2a", "criteria_2b", "criteria_2c", "criteria_2d", "criteria_2e", "criteria_2f", "criteria_2g",
    "criteria_2h", "criteria_3a", "criteria_3b", "criteria_3c", "criteria_3d", "criteria_3e", "criteria_3f",
    "criteria_3g", "criteria_3h", "criteria_3i", "criteria_3j", "criteria_3k", "criteria_3l", "criteria_3m",
    "criteria_3n", "criteria_3o", "criteria_3p", "criteria_3q", "criteria_3r", "criteria_4a", "criteria_4b",
    "criteria_4c", "criteria_4d", "criteria_4e", "criteria_4f", "criteria_4g", "criteria_4h", "criteria_4i",
    "criteria_4j", "criteria_4k", "criteria_4l", "criteria_4m", "criteria_5a", "criteria_5b", "criteria_5c"
  )
}

# Funksjon for å sette opp parallel processing
setup_parallel <- function() {
  n_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(n_cores, type = "PSOCK")
  doParallel::registerDoParallel(cl)
  
  # Eksporter nødvendige funksjoner og data til clusteren
  parallel::clusterExport(cl, c("process_criteria", "test3"))
  
  # Last nødvendige pakker på worker nodes
  parallel::clusterEvalQ(cl, {
    library(tidyr)
    library(krippendorffsalpha)
  })
  
  return(cl)
}

# Funksjon for å kjøre analysen
run_analysis <- function(cl, criteria_columns) {
  results <- parallel::parLapply(
    cl,
    criteria_columns,
    function(col) process_criteria(test3, col)
  )
  return(results)
}

# Funksjon for å formatere resultater
format_results <- function(results, criteria_columns) {
  formatted_results <- data.frame(
    criteria = criteria_columns,
    alpha = sapply(results, function(x) x$alpha$alpha),
    conf_low = sapply(results, function(x) x$confint_default[1]),
    conf_high = sapply(results, function(x) x$confint_default[2]),
    conf_customary_low = sapply(results, function(x) x$confint_customary[1]),
    conf_customary_high = sapply(results, function(x) x$confint_customary[2])
  )
  return(formatted_results)
}

# Funksjon for å kjøre test på ett enkelt kriterie
test_single_criteria <- function(df, criteria) {
  result <- process_criteria(df, criteria)
  print(sprintf("Testing %s:", criteria))
  print("Alpha value:")
  print(result$alpha)
  print("Confidence intervals (default):")
  print(result$confint_default)
  print("Confidence intervals (customary):")
  print(result$confint_customary)
  return(result)
}