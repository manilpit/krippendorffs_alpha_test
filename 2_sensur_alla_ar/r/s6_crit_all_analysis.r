# 6_crit_all_analysis.R
source("4_functions.R")

# Funksjon for å prosessere ett kriterie
simple_test_criteria <- function(df, criteria) {
    message(sprintf("\nProsesserer %s...", criteria))
    message("1. Starter dataseleksjon...")
    df_subset <- df[!is.na(df[[criteria]]), c("kandidatnr", "rater", criteria)]
    message(sprintf("Antall rader med ratings: %d", nrow(df_subset)))
    
    message("2. Utfører pivot_wider...")
    df_wide <- tidyr::pivot_wider(
        df_subset,
        names_from = rater,
        values_from = all_of(criteria),
        values_fill = NA
    )
    
    df_wide <- df_wide[, colSums(!is.na(df_wide)) > 0]
    message(sprintf("Antall ratere: %d", ncol(df_wide) - 1))
    
    message("3. Konverterer til matrise...")
    df_matrix <- as.matrix(df_wide[, -1])
    
    message("4. Beregner Krippendorff's alpha...")
    fit_nom <- krippendorffs.alpha(
        df_matrix,
        level = "nominal",
        confint = TRUE,
        verbose = FALSE,
        control = list(
            bootit = 1000,
            parallel = FALSE
        )
    )
    
    return(fit_nom)
}

# Funksjon for å formatere resultater
format_results <- function(results_list) {
    results_df <- data.frame(
        criteria = names(results_list),
        alpha = sapply(results_list, function(x) x$alpha),
        ci_lower = sapply(results_list, function(x) x$confint[1]),
        ci_upper = sapply(results_list, function(x) x$confint[2])
    )
    
    # Sorter etter kriterie-nummer
    results_df$criteria_num <- as.numeric(gsub("criteria_([0-9]+)[a-z]", "\\1", results_df$criteria))
    results_df$criteria_letter <- gsub("criteria_[0-9]+([a-z])", "\\1", results_df$criteria)
    results_df <- results_df[order(results_df$criteria_num, results_df$criteria_letter), ]
    
    # Fjern hjelpekolonner
    results_df$criteria_num <- NULL
    results_df$criteria_letter <- NULL
    
    return(results_df)
}

# Hent alle kriterier
all_criteria <- names(test3)[grep("^criteria_", names(test3))]
message(sprintf("Fant %d kriterier å analysere", length(all_criteria)))

# Start tidtaking
start_time <- Sys.time()

# Initialize results list
results_list <- list()

# Kjør analyse for hvert kriterie
for(criteria in all_criteria) {
    message(sprintf("\n\nAnalyserer %s (%d/%d)", 
                   criteria, 
                   which(all_criteria == criteria), 
                   length(all_criteria)))
    
    # Kjør analyse
    results_list[[criteria]] <- simple_test_criteria(test3, criteria)
    
    # Print foreløpige resultater
    message(sprintf("Alpha for %s: %.4f (CI: %.4f - %.4f)", 
                   criteria, 
                   results_list[[criteria]]$alpha,
                   results_list[[criteria]]$confint[1],
                   results_list[[criteria]]$confint[2]))
}

# Slutt tidtaking
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

# Formater resultater
formatted_results <- format_results(results_list)
formatted_results$runtime_mins <- as.numeric(total_time)

# Lagre resultater
filename <- paste0("krippendorff_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
write.csv(formatted_results, filename, row.names = FALSE)

# Print sammendrag
message("\n\nAnalyse fullført!")
message(sprintf("Total kjøretid: %.1f minutter", total_time))
message(sprintf("Resultater lagret til: %s", filename))

# Vis sammendragsstatistikk
message("\nSammendrag av alpha-verdier:")
print(summary(formatted_results$alpha))

# Vis kriterier med lav reliabilitet (alpha < 0.67)
low_reliability <- formatted_results[formatted_results$alpha < 0.67, ]
if(nrow(low_reliability) > 0) {
    message("\nKriterier med lav reliabilitet (alpha < 0.67):")
    print(low_reliability[order(low_reliability$alpha), ])
}