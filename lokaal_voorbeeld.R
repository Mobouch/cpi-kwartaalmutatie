# =============================================================================
# lokaal_voorbeeld.R  -  Voorbeeld voor lokale instellingen
#
# Kopieer dit bestand naar 'lokaal.R' (in dezelfde map) en vul je eigen
# gegevens in. 'lokaal.R' staat in .gitignore: servernamen, databases en
# andere verbindingsgegevens komen zo nooit in Git of op GitHub.
# run.R en de Shiny-app laden lokaal.R automatisch als het bestand bestaat.
# =============================================================================

# Resultaten opslaan in SQL Server in plaats van een lokaal SQLite-bestand.
# Gebruik dezelfde ODBC-verbindingsstring als in je andere projecten.
CONFIG$db <- "DRIVER={SQL Server};SERVER=<server>\\<instantie>,<poort>;DATABASE=<database>;"

# In een gedeelde database: geef de tabellen een herkenbaar voorvoegsel,
# zodat ze niet botsen met bestaande tabellen (bijv. casus_cpi_runlog).
CONFIG$tabel_prefix <- "casus_cpi_"

# Optioneel: seizoencorrectie standaard aan.
# CONFIG$seizoencorrectie <- TRUE
