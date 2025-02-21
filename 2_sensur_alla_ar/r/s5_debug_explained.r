# 5_debug_analysis.R
# Dette scriptet tester Krippendorff's alpha for ett enkelt kriterie (criteria_1a)

# 1. Laster inn nødvendige funksjoner fra tidligere script
# source("4_functions.R")

# 2. Definerer hovedfunksjonen som analyserer ett kriterie
simple_test_criteria <- function(df, criteria) {
    # Steg 1: Velger ut relevante data
    message("1. Starter dataseleksjon...")
    df_subset <- df[!is.na(df[[criteria]]), c("kandidatnr", "rater", criteria)]
    print("Antall rader med ratings:")
    print(nrow(df_subset))
    
    # Steg 2: Omformer data fra lang til bred format
    message("\n2. Utfører pivot_wider...")
    df_wide <- tidyr::pivot_wider(
        df_subset,
        names_from = rater,          # Ratere blir kolonner
        values_from = all_of(criteria), # Fyller inn verdiene
        values_fill = NA
    )
    
    # Fjerner tomme kolonner
    df_wide <- df_wide[, colSums(!is.na(df_wide)) > 0]
    print("Rater-kolonner som brukes:")
    print(names(df_wide)[-1])
    
    # Steg 3: Lager matrise for analysen
    message("\n3. Konverterer til matrise...")
    df_matrix <- as.matrix(df_wide[, -1])
    print("Dimensjoner på matrisen:")
    print(dim(df_matrix))
    
    # Steg 4: Beregner Krippendorff's alpha
    message("\n4. Starter Krippendorff's alpha beregning...")
    fit_nom <- krippendorffs.alpha(
        df_matrix,
        level = "nominal",
        confint = TRUE,
        verbose = TRUE,
        control = list(
            bootit = 1000,    # Antall bootstrap-iterasjoner
            parallel = FALSE   # Ikke parallell prosessering
        )
    )
    
    return(fit_nom)
}

# 3. Sjekker dataen før analysen
print("Sjekk av data før analyse:")
print("Antall unike ratere:")
print(table(test3$rater))

print("\nKrysstabell av ratere og verdier for criteria_1a:")
print(table(test3$rater, test3$criteria_1a, useNA = "always"))

# 4. Starter tidtaking og analysen
start_time <- Sys.time()
message("\nStarter test av criteria_1a...")
test_result <- simple_test_criteria(test3, "criteria_1a")

# 5. Viser resultater
print("\nResultater for Krippendorff's alpha:")
print(test_result)

# 6. Beregner total kjøretid
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")
message(sprintf("\nTotal kjøretid: %.1f minutter", total_time))

# 7. Lagrer resultatene
results_summary <- data.frame(
    criteria = "criteria_1a",
    alpha = test_result$alpha,
    ci_lower = test_result$confint[1],
    ci_upper = test_result$confint[2],
    runtime_mins = as.numeric(total_time)
)

# 8. Lagrer til CSV-fil
write.csv(results_summary, "test_results_criteria1a.csv", row.names = FALSE)

# 9. Viser sammendrag
print("\nSammendrag av resultater:")
print(results_summary)