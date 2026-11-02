# =============================================================================
# 01_input.R  -  INPUT
# Haalt CPI-maandcijfers rechtstreeks uit StatLine (CBS Open Data) op.
# Er worden geen externe databestanden (csv/excel) gebruikt.
# =============================================================================

#' Haal CPI-maandcijfers op voor een aantal producten
#'
#' @param tabel_id         StatLine-tabel, standaard "86141NED"
#'                         (Consumentenprijzen; CPI 2025=100, index en mutaties).
#' @param product_patronen Tekstpatronen (reguliere expressies) waarmee de
#'                         producten in de tabel worden gezocht, bijv. "Koffie".
#' @return data.frame met per product en maand het CPI-indexcijfer.
haal_cpi_op <- function(tabel_id = "86141NED", product_patronen) {
  if (!requireNamespace("cbsodataR", quietly = TRUE)) {
    stop("Pakket 'cbsodataR' ontbreekt. Installeer met install.packages('cbsodataR').")
  }

  message("INPUT: metadata ophalen van StatLine-tabel ", tabel_id)
  meta <- cbsodataR::cbs_get_meta(tabel_id)

  # De productdimensie heet in CPI-tabellen 'Bestedingscategorieen'; we zoeken
  # hem op in de metadata zodat de code ook werkt als de naam verandert.
  dimensies   <- meta$DataProperties$Key[meta$DataProperties$Type == "Dimension"]
  product_dim <- setdiff(dimensies, "Perioden")[1]

  categorieen <- zoek_categorieen(meta[[product_dim]], product_patronen)
  cpi_kolom   <- zoek_cpi_kolom(meta$DataProperties)
  message("INPUT: producten = ", paste(categorieen$Title, collapse = ", "),
          " | meetwaarde = ", cpi_kolom)

  # Alleen de gekozen producten en alleen maandcijfers (Perioden met 'MM').
  argumenten <- list(id = tabel_id)
  argumenten[[product_dim]] <- categorieen$Key
  argumenten$Perioden <- cbsodataR::has_substring("MM")
  ruw <- do.call(cbsodataR::cbs_get_data, argumenten)

  maak_maanddata(ruw, product_dim, categorieen, cpi_kolom)
}

#' Zoek per patroon precies een productcategorie in de metadata
zoek_categorieen <- function(categorie_meta, patronen) {
  titels <- trimws(categorie_meta$Title)
  # StatLine-titels beginnen vaak met een code, bijv. "011210 Koffie".
  namen  <- sub("^[0-9]+\\s+", "", titels)
  gevonden <- lapply(patronen, function(patroon) {
    idx <- grep(patroon, namen, ignore.case = TRUE)
    # Bij meerdere treffers wint een exacte naam ("Koffie" boven "Koffie en thee").
    exact <- idx[tolower(namen[idx]) == tolower(patroon)]
    if (length(exact) == 1) idx <- exact
    if (length(idx) == 0) {
      stop("Geen product gevonden voor '", patroon, "'.")
    }
    if (length(idx) > 1) {
      stop("Meerdere producten gevonden voor '", patroon, "': ",
           paste(head(titels[idx], 10), collapse = " | "),
           ". Maak het patroon specifieker.")
    }
    data.frame(Key = trimws(categorie_meta$Key[idx]), Title = titels[idx])
  })
  do.call(rbind, gevonden)
}

#' Zoek de kolom met het CPI-indexcijfer (niet 'afgeleid', geen mutatie of weging)
zoek_cpi_kolom <- function(data_properties) {
  onderwerpen <- data_properties[data_properties$Type == "Topic", ]
  kandidaat <- grepl("^CPI", onderwerpen$Title) &
    !grepl("afgeleid|mutatie|weging", onderwerpen$Title, ignore.case = TRUE)
  if (sum(kandidaat) != 1) {
    stop("Kan de CPI-kolom niet eenduidig vinden. Beschikbaar: ",
         paste(onderwerpen$Title, collapse = " | "))
  }
  onderwerpen$Key[kandidaat]
}

#' Zet de ruwe StatLine-data om naar een nette maandtabel
maak_maanddata <- function(ruw, product_dim, categorieen, cpi_kolom) {
  periode <- trimws(ruw$Perioden)              # bijv. "2026MM09"
  jaar    <- as.integer(substr(periode, 1, 4))
  maand   <- as.integer(substr(periode, 7, 8))
  code    <- trimws(ruw[[product_dim]])

  data.frame(
    product_code = code,
    product      = categorieen$Title[match(code, categorieen$Key)],
    periode      = periode,
    jaar         = jaar,
    maand        = maand,
    kwartaal     = sprintf("%dK%d", jaar, (maand - 1) %/% 3 + 1),
    cpi          = as.numeric(ruw[[cpi_kolom]]),
    stringsAsFactors = FALSE
  )
}
