# Test av ett kriterie
# source("4_functions.R")

# Start tidtaking
start_time <- Sys.time()

# Test kriterie 1a
test_result <- test_single_criteria(test3, "criteria_1a")

# Slutt tidtaking
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

message(sprintf("\nTotal kjøretid: %.1f minutter", total_time))