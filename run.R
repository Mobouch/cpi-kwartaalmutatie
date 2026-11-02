# =============================================================================
# run.R  -  Draait het hele proces: INPUT -> THROUGHPUT -> OUTPUT
#
# Gebruik (vanuit de projectmap):
#   Rscript run.R              # laatste volledige kwartaal
#   Rscript run.R 2026K2       # een gekozen verslagperiode
#   Rscript run.R 2026K2 --sc  # plus seizoengecorrigeerde variant (optioneel)
# =============================================================================

# --- Projectmap vinden ---------------------------------------------------------
# De functies staan in de map R/ naast dit script. Staat de werkmap ergens
# anders (bijv. Documenten), dan zoeken we de map waar run.R zelf staat.
vind_projectmap <- function() {
  is_project <- function(map) dir.exists(file.path(map, "R")) && file.exists(file.path(map, "run.R"))
  if (is_project(getwd())) return(getwd())
  # 1. Via Rscript: Rscript pad/naar/run.R
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg) > 0) {
    map <- dirname(normalizePath(sub("^--file=", "", arg[1])))
    if (is_project(map)) return(map)
  }
  # 2. Via source("pad/naar/run.R")
  for (i in rev(seq_len(sys.nframe()))) {
    bestand <- sys.frame(i)$ofile
    if (!is.null(bestand)) {
      map <- dirname(normalizePath(bestand))
      if (is_project(map)) return(map)
    }
  }
  # 3. Via RStudio: het geopende script
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    pad <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(pad) && is_project(dirname(pad))) return(dirname(normalizePath(pad)))
  }
  stop("Kan de projectmap niet vinden (de map met run.R en de submap R/).\n",
       "Huidige werkmap: ", getwd(), "\n",
       "Oplossing: open 'cpi-kwartaalmutatie.Rproj' in RStudio, of stel de werkmap in met\n",
       "setwd(\"C:/Users/<naam>/Documents/cpi-kwartaalmutatie\") en voer run.R opnieuw uit.")
}

projectmap <- vind_projectmap()
if (!identical(normalizePath(getwd()), projectmap)) {
  setwd(projectmap)
  message("Werkmap ingesteld op de projectmap: ", projectmap)
}

for (bestand in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(bestand, encoding = "UTF-8")
}

# Lokale instellingen (bijv. de verbinding met SQL Server). Dit bestand staat
# in .gitignore en komt dus niet op GitHub. Voorbeeld: lokaal_voorbeeld.R.
if (file.exists("lokaal.R")) source("lokaal.R", encoding = "UTF-8")

draai_proces <- function(verslagperiode = NULL, config = CONFIG,
                         seizoencorrectie = config$seizoencorrectie) {
  controleer_pakketten(c(BENODIGDE_PAKKETTEN, db_pakket(config$db)))
  controleer_outputmap(config$output_map)
  message("Database: ", db_omschrijving(config$db),
          if (nzchar(config$tabel_prefix)) paste0(" (tabellen met voorvoegsel '", config$tabel_prefix, "')") else "")

  # 1. INPUT
  maanddata <- haal_cpi_op(config$tabel_id, config$producten)

  # 2. THROUGHPUT (zoals de casus vraagt: ruwe CPI)
  if (is.null(verslagperiode)) verslagperiode <- laatste_volledige_kwartaal(maanddata)
  verslag      <- bereken_kwartaalmutatie(maanddata, verslagperiode)
  kwartaaldata <- bereken_kwartaal_cpi(maanddata)
  alle_mut     <- bereken_alle_kwartaalmutaties(maanddata)

  # 2b. OPTIONEEL: seizoengecorrigeerde variant
  extra <- list()
  if (isTRUE(seizoencorrectie)) {
    message("THROUGHPUT: seizoencorrectie (", config$sc_methode, ")")
    maand_sc <- seizoencorrigeer(maanddata, config$sc_methode, pakket = config$sc_pakket)
    extra$cpi_maand_sc        <- maand_sc
    extra$kwartaalmutaties_sc <- bereken_alle_kwartaalmutaties(maand_sc)
    verslag$mutatie_pct_sc <- extra$kwartaalmutaties_sc$mutatie_pct[
      match(paste(verslag$product_code, verslagperiode),
            paste(extra$kwartaalmutaties_sc$product_code, extra$kwartaalmutaties_sc$verslagperiode))]
  }

  # 3. OUTPUT
  maak_grafieken(maanddata, alle_mut, config$output_map)
  if (isTRUE(seizoencorrectie)) {
    vergelijking <- rbind(transform(alle_mut, reeks = "Ruw"),
                          transform(extra$kwartaalmutaties_sc, reeks = "Seizoengecorrigeerd"))
    ggplot2::ggsave(file.path(config$output_map, "kwartaalmutatie_ruw_vs_sc.png"),
                    grafiek_ruw_vs_sc(vergelijking), width = 8, height = 6, dpi = 150, bg = "white")
  }
  schrijf_naar_database(config$db, maanddata, kwartaaldata, alle_mut,
                        verslag, config$tabel_id, extra)

  toon <- verslag[, intersect(c("product", "cpi_kwartaal", "cpi_vorig_kwartaal",
                                "mutatie_pct", "mutatie_pct_sc"), names(verslag))]
  toon[-1] <- lapply(toon[-1], round, 2)
  print(toon)
  invisible(verslag)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  periode <- args[grepl("^[0-9]{4}K[1-4]$", args)]
  draai_proces(if (length(periode) > 0) periode[1] else NULL,
               seizoencorrectie = "--sc" %in% args || isTRUE(CONFIG$seizoencorrectie))
}
