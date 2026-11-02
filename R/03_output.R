# =============================================================================
# 03_output.R  -  OUTPUT
# Maakt grafieken en slaat de resultaten op in een database:
# een lokaal SQLite-bestand of SQL Server via ODBC.
# =============================================================================

#' Grafiek: maandelijkse CPI per product
grafiek_maand_cpi <- function(maanddata) {
  maanddata$datum <- as.Date(sprintf("%d-%02d-01", maanddata$jaar, maanddata$maand))
  ggplot2::ggplot(maanddata, ggplot2::aes(datum, cpi, colour = product)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(title = "CPI per maand", x = NULL, y = "CPI (2025=100)",
                  colour = NULL, caption = "Bron: CBS StatLine") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")
}

#' Grafiek: kwartaalmutatie per product
grafiek_kwartaalmutatie <- function(mutaties, laatste_n = 12) {
  kwartalen <- tail(sort(unique(mutaties$verslagperiode)), laatste_n)
  data <- mutaties[mutaties$verslagperiode %in% kwartalen, ]
  ggplot2::ggplot(data, ggplot2::aes(verslagperiode, mutatie_pct, fill = product)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::labs(title = "Kwartaalmutatie CPI",
                  subtitle = "% t.o.v. voorgaand kwartaal",
                  x = NULL, y = "%", fill = NULL, caption = "Bron: CBS StatLine") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Sla de grafieken op als PNG en geef de bestandspaden terug
maak_grafieken <- function(maanddata, mutaties, map = "output") {
  dir.create(map, showWarnings = FALSE, recursive = TRUE)
  paden <- c(maand   = file.path(map, "cpi_per_maand.png"),
             mutatie = file.path(map, "kwartaalmutatie.png"))
  ggplot2::ggsave(paden["maand"], grafiek_maand_cpi(maanddata),
                  width = 8, height = 4.5, dpi = 150, bg = "white")
  ggplot2::ggsave(paden["mutatie"], grafiek_kwartaalmutatie(mutaties),
                  width = 8, height = 4.5, dpi = 150, bg = "white")
  message("OUTPUT: grafieken opgeslagen in ", map)
  paden
}

# --- Database: SQLite of SQL Server ------------------------------------------
# CONFIG$db is een pad naar een .sqlite-bestand of een ODBC-verbindingsstring
# ("DRIVER={SQL Server};SERVER=...;DATABASE=...;"). Alle functies hieronder
# werken met beide; de rest van het proces merkt het verschil niet.

#' Is de database een ODBC-verbinding (bijv. SQL Server)?
is_odbc <- function(db) grepl("^\\s*DRIVER\\s*=", db, ignore.case = TRUE)

#' Welk R-pakket is nodig voor deze database?
db_pakket <- function(db) if (is_odbc(db)) "odbc" else "RSQLite"

#' Leesbare omschrijving zonder servernaam of andere verbindingsgegevens
db_omschrijving <- function(db) {
  if (is_odbc(db)) "SQL Server (ODBC)" else normalizePath(db, mustWork = FALSE)
}

#' Maak een verbinding met de database
maak_verbinding <- function(db) {
  if (is_odbc(db)) {
    DBI::dbConnect(odbc::odbc(), .connection_string = db)
  } else {
    dir.create(dirname(db), showWarnings = FALSE, recursive = TRUE)
    DBI::dbConnect(RSQLite::SQLite(), db)
  }
}

#' Tabelnaam met het voorvoegsel uit de configuratie (bijv. "casus_cpi_runlog")
tabelnaam <- function(naam, prefix = CONFIG$tabel_prefix) paste0(if (is.null(prefix)) "" else prefix, naam)

#' Schrijf alle resultaten naar de database.
#' Tabellen worden bij elke run volledig ververst (reproduceerbaar en
#' idempotent); de tabel 'runlog' houdt bij welke runs er zijn geweest.
schrijf_naar_database <- function(db, maanddata, kwartaaldata, mutaties,
                                  verslag, tabel_id, extra = list()) {
  con <- maak_verbinding(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, tabelnaam("cpi_maand"),        as.data.frame(maanddata),    overwrite = TRUE)
    DBI::dbWriteTable(con, tabelnaam("cpi_kwartaal"),     as.data.frame(kwartaaldata), overwrite = TRUE)
    DBI::dbWriteTable(con, tabelnaam("kwartaalmutaties"), as.data.frame(mutaties),     overwrite = TRUE)
    # Optionele extra tabellen (bijv. seizoengecorrigeerd). Zonder extra's
    # worden oude extra tabellen verwijderd, zodat de database klopt met de run.
    for (oud in c("cpi_maand_sc", "kwartaalmutaties_sc")) {
      if (!oud %in% names(extra) && DBI::dbExistsTable(con, tabelnaam(oud))) {
        DBI::dbRemoveTable(con, tabelnaam(oud))
      }
    }
    for (naam in names(extra)) {
      DBI::dbWriteTable(con, tabelnaam(naam), as.data.frame(extra[[naam]]), overwrite = TRUE)
    }
    DBI::dbWriteTable(con, tabelnaam("runlog"), data.frame(
      tijdstip       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      bron           = tabel_id,
      verslagperiode = unique(verslag$verslagperiode),
      producten      = paste(verslag$product, collapse = "; "),
      n_maandrecords = nrow(maanddata)
    ), append = TRUE)
  })
  message("OUTPUT: resultaten opgeslagen in ", db_omschrijving(db))
  invisible(db)
}

#' Bestaat een tabel in de database?
tabel_bestaat <- function(db, tabel) {
  con <- maak_verbinding(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExistsTable(con, tabelnaam(tabel))
}

#' Lees een hele tabel uit de database (gebruikt door de Shiny-app)
lees_uit_database <- function(db, tabel) {
  con <- maak_verbinding(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbReadTable(con, tabelnaam(tabel))
}

#' Lees de kwartaalmutaties van een verslagperiode met een SQL-query.
#' De verslagperiode gaat als parameter mee (geen tekst in de query plakken),
#' zodat de query veilig is en op SQLite en SQL Server gelijk werkt.
lees_mutaties <- function(db, verslagperiode, seizoengecorrigeerd = FALSE) {
  controleer_verslagperiode(verslagperiode)
  con <- maak_verbinding(db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabel <- DBI::dbQuoteIdentifier(con, tabelnaam(if (seizoengecorrigeerd) "kwartaalmutaties_sc" else "kwartaalmutaties"))
  sql <- paste("SELECT verslagperiode, product_code, product, cpi_kwartaal,",
               "cpi_vorig_kwartaal, mutatie_pct FROM", tabel,
               "WHERE verslagperiode = ? ORDER BY product")
  DBI::dbGetQuery(con, sql, params = list(verslagperiode))
}
