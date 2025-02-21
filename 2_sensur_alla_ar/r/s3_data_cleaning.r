# # Data cleaning script
# source("2_data_import.R")

# # Rens h22 data
# h22t <- h22 %>%
#   select(-Resultat, -Poengsum, -sensor_kandidatnr, -"5d") %>%
#   mutate(across(c("4g", "4j"), ~ ifelse(. == "", NA, .))) %>%
#   mutate(across(c("4g", "4j"), as.numeric)) %>%
#   filter(sensor != "samsensur")

# # Legg til "criteria" prefix
# colnames(h22t) <- colnames(h22t) %>%
#   str_replace_all("^([0-9])", "criteria_\\1")

# # Fjern NA-verdier
# h22_klar <- clean_na_values(h22t) # Definer denne funksjonen

# # Lag test3 datasett
# test3 <- prepare_test_data(h22_klar) # Definer denne funksjonen


# 3_data_cleaning.R
# source("2_data_import.R")

# Funksjon for å fjerne NA-verdier
clean_na_values <- function(df) {
  # Finn kandidater med NA i både sensor a og b
  candidates_with_both_na <- df %>%
    group_by(kandidatnr) %>%
    filter(
      any(sensor == "sensor_a" & rowSums(is.na(across(starts_with("criteria")))) == ncol(across(starts_with("criteria")))) &
      any(sensor == "sensor_b" & rowSums(is.na(across(starts_with("criteria")))) == ncol(across(starts_with("criteria"))))
    ) %>%
    ungroup() %>%
    select(kandidatnr) %>%
    distinct()
  
  # Fjern kandidater med NA i begge sensorer
  df_filtered <- df %>%
    filter(!kandidatnr %in% candidates_with_both_na$kandidatnr)
  
  # Finn kandidater med NA i én av sensorene
  candidates_with_na <- df_filtered %>%
    group_by(kandidatnr) %>%
    filter(
      any(sensor == "sensor_a" & rowSums(is.na(across(starts_with("criteria")))) == ncol(across(starts_with("criteria")))) |
      any(sensor == "sensor_b" & rowSums(is.na(across(starts_with("criteria")))) == ncol(across(starts_with("criteria"))))
    ) %>%
    ungroup()
  
  # Fjern kandidater med NA i én sensor
  df_clean <- df_filtered %>%
    filter(!kandidatnr %in% candidates_with_na$kandidatnr)
  
  return(df_clean)
}

# Funksjon for å preparere test data
prepare_test_data <- function(df) {
  # Fikse Sensorpar-verdier
  df$Sensorpar <- gsub("4 AC", "4ac", df$Sensorpar)
  df$Sensorpar <- gsub("4CD", "4cd", df$Sensorpar)
  
  # Lag rater-variabel
  df$rater <- df$Sensorpar
  
  # Separer ratere
  for (i in 1:nrow(df)) {
    if (df$sensor[i] == "sensor_a") {
      df$rater[i] <- substr(df$rater[i], 1, nchar(df$rater[i])-1)
    } else if (df$sensor[i] == "sensor_b" && nchar(df$rater[i]) > 1) {
      df$rater[i] <- paste0(substr(df$rater[i], 1, nchar(df$rater[i])-2),
                           substr(df$rater[i], nchar(df$rater[i]), nchar(df$rater[i])))
    }
  }
  
  # Fjern unødvendige kolonner og velg relevante
  df <- df %>%
    select(!Sensorpar) %>%
    select(!sensor)
  
  return(df)
}

# Hovedprosess for datarensing
clean_h22_data <- function(h22) {
  # Rens h22 data
  h22t <- h22 %>%
    select(-Resultat, -Poengsum, -sensor_kandidatnr, -"5d") %>%
    mutate(across(c("4g", "4j"), ~ ifelse(. == "", NA, .))) %>%
    mutate(across(c("4g", "4j"), as.numeric)) %>%
    filter(sensor != "samsensur")
  
  # Legg til "criteria" prefix
  colnames(h22t) <- colnames(h22t) %>%
    str_replace_all("^([0-9])", "criteria_\\1")
  
  # Fjern NA-verdier
  h22_klar <- clean_na_values(h22t)
  
  # Lag test3 datasett
  test3 <- prepare_test_data(h22_klar)
  
  # Print info om datarensingen
  message("Original antall kandidater: ", n_distinct(h22$kandidatnr))
  message("Antall kandidater etter rensing: ", n_distinct(test3$kandidatnr))
  
  return(list(
    h22t = h22t,
    h22_klar = h22_klar,
    test3 = test3
  ))
}

# Kjør datarensingen
cleaned_data <- clean_h22_data(h22)

# Tilordne resultater til separate variabler
h22t <- cleaned_data$h22t
h22_klar <- cleaned_data$h22_klar
test3 <- cleaned_data$test3