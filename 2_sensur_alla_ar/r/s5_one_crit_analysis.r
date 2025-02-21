# 5_debug_analysis.R
# Test/debug versjon av analysen for et enkelt kriterie

# Last funksjoner og data
# source("4_functions.R")

# Forbedret versjon som bare bruker faktiske ratings
simple_test_criteria <- function(df, criteria) {
    message("1. Starter dataseleksjon...")
    # Velg bare rader hvor vi har faktiske ratings
    df_subset <- df[!is.na(df[[criteria]]), c("kandidatnr", "rater", criteria)]
    print("Antall rader med ratings:")
    print(nrow(df_subset))
    
    message("\n2. Utfører pivot_wider...")
    df_wide <- tidyr::pivot_wider(
        df_subset,
        names_from = rater,
        values_from = all_of(criteria),
        values_fill = NA
    )
    
    # Fjern kolonner som bare har NA
    df_wide <- df_wide[, colSums(!is.na(df_wide)) > 0]
    print("Rater-kolonner som brukes:")
    print(names(df_wide)[-1])  # -1 for å ikke vise kandidatnr
    
    message("\n3. Konverterer til matrise...")
    df_matrix <- as.matrix(df_wide[, -1])
    print("Dimensjoner på matrisen:")
    print(dim(df_matrix))
    
    message("\n4. Starter Krippendorff's alpha beregning...")
    fit_nom <- krippendorffs.alpha(
        df_matrix,
        level = "nominal",
        confint = TRUE,
        verbose = TRUE,
        control = list(
            bootit = 1000,
            parallel = FALSE
        )
    )
    
    return(fit_nom)
}

# Data sjekk før analyse
print("Sjekk av data før analyse:")
print("Antall unike ratere:")
print(table(test3$rater))

print("\nKrysstabell av ratere og verdier for criteria_1a:")
print(table(test3$rater, test3$criteria_1a, useNA = "always"))

# Start tidtaking
start_time <- Sys.time()

message("\nStarter test av criteria_1a...")

# Test med forbedret versjon
test_result <- simple_test_criteria(test3, "criteria_1a")

# Print resultater
print("\nResultater for Krippendorff's alpha:")
print(test_result)

# Slutt tidtaking
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

message(sprintf("\nTotal kjøretid: %.1f minutter", total_time))

# Lagre resultater
results_summary <- data.frame(
    criteria = "criteria_1a",
    alpha = test_result$alpha,
    ci_lower = test_result$confint[1],
    ci_upper = test_result$confint[2],
    runtime_mins = as.numeric(total_time)
)

# Lagre til CSV
write.csv(results_summary, "test_results_criteria1a.csv", row.names = FALSE)

# Print sammendrag
print("\nSammendrag av resultater:")
print(results_summary)