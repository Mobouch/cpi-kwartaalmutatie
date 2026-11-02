# Testreeks met een bekend seizoenpatroon: trend * seizoenfactor.
# Na correctie moet de kwartaalmutatie dicht bij de groei van de trend liggen.
maak_seizoensreeks <- function(jaren = 2021:2025) {
  d <- expand.grid(maand = 1:12, jaar = jaren)
  d <- d[order(d$jaar, d$maand), ]
  t <- seq_len(nrow(d))
  seizoen <- 1 + 0.04 * sin(2 * pi * d$maand / 12)        # +-4% seizoenpatroon
  data.frame(product_code = "S", product = "Seizoensproduct",
             periode  = sprintf("%dMM%02d", d$jaar, d$maand),
             jaar = d$jaar, maand = d$maand,
             kwartaal = sprintf("%dK%d", d$jaar, (d$maand - 1) %/% 3 + 1),
             cpi = 100 * 1.002^t * seizoen)                  # trend +0,2% per maand
}

test_that("seizoencorrectie haalt het seizoenpatroon uit de kwartaalmutatie", {
  data <- maak_seizoensreeks()
  trendgroei <- 100 * (1.002^3 - 1)                          # ca. 0,6% per kwartaal
  ruw <- bereken_kwartaalmutatie(data, "2024K3")$mutatie_pct
  sc  <- bereken_kwartaalmutatie_sc(data, "2024K3", methode = "decompose")$mutatie_pct
  expect_gt(abs(ruw - trendgroei), 1)                        # ruw zit er ver naast
  expect_lt(abs(sc - trendgroei), 0.1)                       # gecorrigeerd niet
})

test_that("gecorrigeerde data houdt de ruwe waarde bij", {
  sc <- seizoencorrigeer(maak_seizoensreeks(), "decompose")
  expect_equal(sc$cpi_ruw, maak_seizoensreeks()$cpi)
  expect_false(isTRUE(all.equal(sc$cpi, sc$cpi_ruw)))
})

test_that("te korte reeks of reeks met gaten geeft een fout", {
  expect_error(seizoencorrigeer(maak_seizoensreeks(2024:2025), "decompose"), "korter dan")
  data <- maak_seizoensreeks()
  expect_error(seizoencorrigeer(data[-10, ], "decompose"), "gaten")
})

test_that("maandmutatie wordt goed berekend", {
  mm <- bereken_maandmutatie(maak_testdata())
  a  <- mm[mm$product_code == "A", ]
  expect_true(is.na(a$mutatie_pct[1]))
  expect_equal(a$mutatie_pct[2], 100 * (101 / 100 - 1))
})

test_that("seizoengecorrigeerde tabellen komen in SQLite en verdwijnen zonder --sc", {
  skip_if_not_installed("RSQLite")
  data <- maak_seizoensreeks()
  sc   <- seizoencorrigeer(data, "decompose")
  db   <- tempfile(fileext = ".sqlite")
  args <- list(db, data, bereken_kwartaal_cpi(data), bereken_alle_kwartaalmutaties(data),
               bereken_kwartaalmutatie(data, "2025K4"), "TEST")
  do.call(schrijf_naar_database, c(args, list(extra = list(
    cpi_maand_sc = sc, kwartaalmutaties_sc = bereken_alle_kwartaalmutaties(sc)))))
  expect_true(tabel_bestaat(db, "kwartaalmutaties_sc"))
  do.call(schrijf_naar_database, args)
  expect_false(tabel_bestaat(db, "kwartaalmutaties_sc"))
})
