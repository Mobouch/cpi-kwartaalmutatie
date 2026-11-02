# =============================================================================
# 02_throughput.R  -  THROUGHPUT
# Berekent per kwartaal een CPI (gemiddelde van drie maanden) en daarna de
# kwartaalmutatie: de procentuele ontwikkeling t.o.v. het voorgaande kwartaal.
# =============================================================================

#' Controleer of een verslagperiode de vorm "JJJJKn" heeft, bijv. "2026K3"
controleer_verslagperiode <- function(verslagperiode) {
  if (!is.character(verslagperiode) || length(verslagperiode) != 1 ||
      !grepl("^[0-9]{4}K[1-4]$", verslagperiode)) {
    stop("Verslagperiode moet de vorm 'JJJJKn' hebben, bijv. '2026K3'.")
  }
  invisible(TRUE)
}

#' Geef het kwartaal voorafgaand aan de verslagperiode ("2026K1" -> "2025K4")
vorig_kwartaal <- function(verslagperiode) {
  controleer_verslagperiode(verslagperiode)
  jaar <- as.integer(substr(verslagperiode, 1, 4))
  kw   <- as.integer(substr(verslagperiode, 6, 6))
  if (kw == 1) sprintf("%dK4", jaar - 1) else sprintf("%dK%d", jaar, kw - 1)
}

#' Kwartaal-CPI = gemiddelde van de drie maandcijfers.
#' Kwartalen met minder dan drie (geldige) maanden vallen af.
bereken_kwartaal_cpi <- function(maanddata) {
  maanddata |>
    dplyr::filter(!is.na(cpi)) |>
    dplyr::group_by(product_code, product, kwartaal) |>
    dplyr::summarise(cpi_kwartaal = mean(cpi),
                     n_maanden    = dplyr::n(),
                     .groups      = "drop") |>
    dplyr::filter(n_maanden == 3) |>
    dplyr::select(-n_maanden) |>
    dplyr::arrange(product, kwartaal)
}

#' Kwartaalmutatie voor een verslagperiode
#'
#' @param maanddata      Uitkomst van haal_cpi_op().
#' @param verslagperiode Kwartaal als "JJJJKn", bijv. "2026K3".
#' @return data.frame met per product de kwartaal-CPI van de verslagperiode,
#'         die van het vorige kwartaal en de kwartaalmutatie in procenten.
bereken_kwartaalmutatie <- function(maanddata, verslagperiode) {
  controleer_verslagperiode(verslagperiode)
  vorige <- vorig_kwartaal(verslagperiode)
  kwartalen <- bereken_kwartaal_cpi(maanddata)

  huidig <- dplyr::filter(kwartalen, kwartaal == verslagperiode)
  vorig  <- dplyr::filter(kwartalen, kwartaal == vorige)
  if (nrow(huidig) == 0) {
    stop("Geen drie maandcijfers beschikbaar voor verslagperiode ", verslagperiode, ".")
  }
  if (nrow(vorig) == 0) {
    stop("Geen drie maandcijfers beschikbaar voor het vorige kwartaal ", vorige, ".")
  }

  dplyr::inner_join(
    dplyr::select(huidig, product_code, product, cpi_kwartaal),
    dplyr::select(vorig,  product_code, cpi_vorig_kwartaal = cpi_kwartaal),
    by = "product_code"
  ) |>
    dplyr::mutate(
      verslagperiode = verslagperiode,
      vorig_kwartaal = vorige,
      mutatie_pct    = 100 * (cpi_kwartaal / cpi_vorig_kwartaal - 1)
    ) |>
    dplyr::select(verslagperiode, vorig_kwartaal, product_code, product,
                  cpi_kwartaal, cpi_vorig_kwartaal, mutatie_pct)
}

#' Kwartaalmutaties voor alle kwartalen waarvoor dat kan (voor grafieken)
bereken_alle_kwartaalmutaties <- function(maanddata) {
  kwartalen <- sort(unique(bereken_kwartaal_cpi(maanddata)$kwartaal))
  resultaten <- lapply(kwartalen, function(k) {
    tryCatch(bereken_kwartaalmutatie(maanddata, k), error = function(e) NULL)
  })
  dplyr::bind_rows(resultaten)
}

#' Laatste kwartaal met drie maandcijfers (standaard verslagperiode)
laatste_volledige_kwartaal <- function(maanddata) {
  max(bereken_kwartaal_cpi(maanddata)$kwartaal)
}
