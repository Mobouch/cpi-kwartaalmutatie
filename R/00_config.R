# =============================================================================
# 00_config.R  -  Instellingen op een plek
# =============================================================================

# Uitvoermap: standaard 'output' in de projectmap. Mag R daar niet schrijven,
# kies dan een andere map met de omgevingsvariabele CPI_OUTPUT, bijv. in R:
#   Sys.setenv(CPI_OUTPUT = "C:/Users/<naam>/Documents/cpi_output")
OUTPUT_MAP <- Sys.getenv("CPI_OUTPUT", "output")

CONFIG <- list(
  # StatLine-tabel: Consumentenprijzen; CPI 2025=100, index en mutaties
  tabel_id  = "86141NED",
  # Twee producten (zoekpatronen op de naam in StatLine)
  producten = c("Koffie", "Elektriciteit"),
  # Uitvoer: grafieken in output_map; resultaten in een database.
  # Standaard een lokaal SQLite-bestand. Voor SQL Server zet je in het
  # (niet in Git opgenomen) bestand lokaal.R een ODBC-verbindingsstring,
  # zie lokaal_voorbeeld.R. Verbindingsgegevens staan dus nooit in de code.
  db           = file.path(OUTPUT_MAP, "cpi.sqlite"),
  tabel_prefix = "",          # bijv. "casus_cpi_" in een gedeelde database
  output_map   = OUTPUT_MAP,
  # Optionele extra: seizoengecorrigeerde variant naast de ruwe mutatie.
  # Aanzetten hier of met: Rscript run.R --sc
  seizoencorrectie = FALSE,
  sc_methode       = "jdemetra",
  # R-pakket voor JDemetra+: "auto", "rjd3x13" (versie 3) of "RJDemetra" (versie 2)
  sc_pakket        = "auto"
)

BENODIGDE_PAKKETTEN <- c("cbsodataR", "dplyr", "ggplot2", "DBI")
# Het databasepakket (RSQLite of odbc) hangt af van CONFIG$db; zie db_pakket().

#' Maak de uitvoermap aan en controleer of R erin mag schrijven.
#' Wordt aan het begin van het proces aangeroepen, zodat een fout direct
#' zichtbaar is en niet pas na het ophalen en rekenen.
controleer_outputmap <- function(map) {
  if (!dir.exists(map)) dir.create(map, recursive = TRUE, showWarnings = FALSE)
  testbestand <- file.path(map, ".schrijftest")
  kan_schrijven <- dir.exists(map) &&
    isTRUE(tryCatch(file.create(testbestand, showWarnings = FALSE), error = function(e) FALSE))
  if (!kan_schrijven) {
    stop("R mag niet schrijven in de uitvoermap '", normalizePath(map, mustWork = FALSE), "'.\n",
         "Huidige werkmap: ", getwd(), "\n",
         "Oplossing: pak de zip uit naar een eigen map (bijv. Documenten) en open daar het project,\n",
         "of kies een andere uitvoermap: Sys.setenv(CPI_OUTPUT = \"C:/Users/<naam>/Documents/cpi_output\")\n",
         "en laad daarna run.R opnieuw.")
  }
  unlink(testbestand)
  invisible(TRUE)
}

controleer_pakketten <- function(pakketten = BENODIGDE_PAKKETTEN) {
  ontbreekt <- pakketten[!vapply(pakketten, requireNamespace, logical(1), quietly = TRUE)]
  if (length(ontbreekt) > 0) {
    stop("Ontbrekende pakketten: ", paste(ontbreekt, collapse = ", "),
         "\nInstalleer met: install.packages(c(",
         paste0("'", ontbreekt, "'", collapse = ", "), "))")
  }
  invisible(TRUE)
}
