# Sjekk av data for criteria_1a
print("Antall unike ratere:")
print(table(test3$rater))

print("\nKrysstabell av ratere og verdier for criteria_1a:")
print(table(test3$rater, test3$criteria_1a, useNA = "always"))

# Sjekk om vi har riktig struktur på test3
print("\nStruktur av test3:")
str(test3)

# Sjekk første få rader av originaldataen
print("\nFørste rader av test3:")
head(test3)