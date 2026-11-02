# =============================================================================
# 04_seizoencorrectie.R  -  OPTIONELE EXTRA (THROUGHPUT)
#
# De casus vraagt om de kwartaalmutatie van de ruwe CPI. Die bevat ook
# seizoeninvloeden: een kwartaalgemiddelde haalt het seizoenpatroon niet weg.
# Deze module maakt daarom een seizoengecorrigeerde variant, naast de ruwe.
#
# WAAROM JDEMETRA+?
# - Europese standaard: binnen het Europees Statistisch Systeem (ESS) is
#   JDemetra+ sinds 2015 de aanbevolen software voor seizoencorrectie.
#   Eurostat liet het ontwikkelen met de Nationale Bank van Belgie en de
#   Bundesbank. Zo wordt hier gecorrigeerd zoals het CBS dat in productie doet.
# - Erkende methoden: X13-ARIMA-SEATS en TRAMO-SEATS, met automatische
#   modelkeuze, uitbijterdetectie en diagnostiek.
# - Reproduceerbaar: de specificatie staat in code; iedereen die dit script
#   draait, krijgt dezelfde uitkomst (anders dan handwerk in de grafische
#   JDemetra-omgeving).
#
# WELK R-PAKKET? (instelbaar via CONFIG$sc_pakket)
# - "rjd3x13"   : koppeling met JDemetra+ versie 3 (actueel; vraagt Java 21+).
# - "RJDemetra" : koppeling met JDemetra+ versie 2 (oudere versie; voor
#                 werkplekken met een oudere Java of JDemetra+ 2 in productie).
# - "auto"      : rjd3x13 als die geinstalleerd is, anders RJDemetra.
# Beide pakketten roepen dezelfde software aan: JDemetra+.
#
# ALTERNATIEVEN (bewust niet gekozen)
# - 'seasonal' (X-13ARIMA-SEATS van het US Census Bureau): geen Java nodig,
#   maar niet de ESS-standaard.
# - stats::decompose / stl (basis-R): geen ARIMA-model, geen uitbijters, geen
#   diagnostiek; niet geschikt voor officiele cijfers. 'decompose' wordt hier
#   alleen gebruikt in de tests, zodat die zonder Java draaien.
#
# SPECIFICATIE: X13, RSA3 = automatische keuze log/niveau, uitbijters en
# ARIMA-model, zonder werkdag- of paascorrectie (voor prijzen niet zinvol).
# =============================================================================

#' Seizoencorrigeer de maand-CPI per product
#'
#' @param maanddata Uitkomst van haal_cpi_op().
#' @param methode   "jdemetra" (standaard) of "decompose" (alleen voor tests).
#' @param pakket    "auto", "rjd3x13" of "RJDemetra" (zie toelichting boven).
#' @param min_jaren Minimale reekslengte in jaren (X13 vraagt er minstens 3).
#' @return maanddata met in 'cpi' de seizoengecorrigeerde waarde en in
#'         'cpi_ruw' de oorspronkelijke waarde. Zo kunnen dezelfde
#'         kwartaalfuncties uit 02_throughput.R worden hergebruikt.
seizoencorrigeer <- function(maanddata, methode = c("jdemetra", "decompose"),
                             min_jaren = 3, pakket = "auto") {
  methode <- match.arg(methode)
  per_product <- split(maanddata, maanddata$product_code)

  resultaat <- lapply(per_product, function(d) {
    d <- d[order(d$jaar, d$maand), ]
    controleer_reeks(d, min_jaren)
    y  <- stats::ts(d$cpi, start = c(d$jaar[1], d$maand[1]), frequency = 12)
    sa <- switch(methode,
                 jdemetra  = sa_jdemetra(y, pakket),
                 decompose = sa_decompose(y))
    if (length(sa) != length(y) || anyNA(sa)) {
      stop("Seizoencorrectie gaf geen volledige reeks terug voor ", d$product[1], ".")
    }
    d$cpi_ruw <- d$cpi
    d$cpi     <- as.numeric(sa)
    d
  })
  out <- do.call(rbind, resultaat)
  rownames(out) <- NULL
  out
}

#' Een reeks moet aaneengesloten zijn, zonder lege waarden en lang genoeg
controleer_reeks <- function(d, min_jaren) {
  if (anyNA(d$cpi)) stop("Lege CPI-waarden in de reeks van ", d$product[1], ".")
  maandnummer <- d$jaar * 12 + d$maand
  if (any(diff(maandnummer) != 1)) stop("De reeks van ", d$product[1], " heeft gaten.")
  if (nrow(d) < 12 * min_jaren) {
    stop("De reeks van ", d$product[1], " is korter dan ", min_jaren, " jaar.")
  }
  invisible(TRUE)
}

#' X13 via JDemetra+ met het gekozen R-pakket.
#' Alleen deze functie hangt af van het pakket; de rest van het proces niet.
sa_jdemetra <- function(y, pakket = c("auto", "rjd3x13", "RJDemetra")) {
  pakket <- match.arg(pakket)
  if (pakket == "auto") {
    pakket <- if (requireNamespace("rjd3x13", quietly = TRUE)) "rjd3x13"
              else if (requireNamespace("RJDemetra", quietly = TRUE)) "RJDemetra"
              else stop("Voor seizoencorrectie met JDemetra+ is 'rjd3x13' of 'RJDemetra' nodig (plus Java). ",
                        "Installeer bijv. install.packages('rjd3x13').")
  }
  if (!requireNamespace(pakket, quietly = TRUE)) {
    stop("Pakket '", pakket, "' is niet geinstalleerd. Installeer met install.packages('", pakket, "').")
  }
  message("THROUGHPUT: seizoencorrectie met JDemetra+ via ", pakket)
  if (pakket == "rjd3x13") {
    model <- rjd3x13::x13(y, spec = "rsa3")
    sa <- model$result$final$d11final            # D11 = seizoengecorrigeerde reeks
    if (is.null(sa)) stop("Onverwachte uitvoer van rjd3x13::x13(); zie ?rjd3x13::x13.")
    return(sa)
  }
  model <- RJDemetra::x13(y, spec = "RSA3")    # JDemetra+ 2: spec in hoofdletters
  model$final$series[, "sa"]
}

#' Klassieke multiplicatieve decompositie (alleen ter controle)
sa_decompose <- function(y) {
  y / stats::decompose(y, type = "multiplicative")$seasonal
}

#' Kwartaalmutatie op basis van de seizoengecorrigeerde reeks
bereken_kwartaalmutatie_sc <- function(maanddata, verslagperiode, methode = "jdemetra",
                                       pakket = "auto") {
  bereken_kwartaalmutatie(seizoencorrigeer(maanddata, methode, pakket = pakket), verslagperiode)
}

#' Maand-op-maandmutatie (%) per product; werkt op ruwe en gecorrigeerde data
bereken_maandmutatie <- function(maanddata) {
  maanddata |>
    dplyr::arrange(product_code, jaar, maand) |>
    dplyr::group_by(product_code) |>
    dplyr::mutate(
      vorige_maand = dplyr::lag(jaar * 12 + maand),
      mutatie_pct  = ifelse(jaar * 12 + maand - vorige_maand == 1,
                            100 * (cpi / dplyr::lag(cpi) - 1), NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(product_code, product, periode, jaar, maand, cpi, mutatie_pct)
}

#' Ruwe en seizoengecorrigeerde kwartaalmutaties onder elkaar, voor vergelijking
vergelijk_ruw_en_sc <- function(maanddata, methode = "jdemetra") {
  ruw <- bereken_alle_kwartaalmutaties(maanddata)
  sc  <- bereken_alle_kwartaalmutaties(seizoencorrigeer(maanddata, methode))
  ruw$reeks <- "Ruw"
  sc$reeks  <- "Seizoengecorrigeerd"
  rbind(ruw, sc)
}

#' Grafiek: ruwe en seizoengecorrigeerde kwartaalmutatie per product
grafiek_ruw_vs_sc <- function(vergelijking, laatste_n = 12) {
  kwartalen <- tail(sort(unique(vergelijking$verslagperiode)), laatste_n)
  data <- vergelijking[vergelijking$verslagperiode %in% kwartalen, ]
  ggplot2::ggplot(data, ggplot2::aes(verslagperiode, mutatie_pct, fill = reeks)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::facet_wrap(~ product, ncol = 1) +
    ggplot2::scale_fill_manual(values = c(Ruw = "grey70", Seizoengecorrigeerd = "#1f6f8b")) +
    ggplot2::labs(title = "Kwartaalmutatie CPI: ruw en seizoengecorrigeerd",
                  subtitle = "% t.o.v. voorgaand kwartaal",
                  x = NULL, y = "%", fill = NULL,
                  caption = "Bron: CBS StatLine; seizoencorrectie: eigen berekening") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
