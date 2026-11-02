test_that("verslagperiode wordt gecontroleerd", {
  expect_silent(controleer_verslagperiode("2026K3"))
  expect_error(controleer_verslagperiode("2026Q3"), "JJJJKn")
  expect_error(controleer_verslagperiode("2026K5"), "JJJJKn")
})

test_that("vorig kwartaal gaat goed over de jaargrens", {
  expect_equal(vorig_kwartaal("2026K1"), "2025K4")
  expect_equal(vorig_kwartaal("2026K3"), "2026K2")
})

test_that("kwartaal-CPI is het gemiddelde van drie maanden", {
  kw <- bereken_kwartaal_cpi(maak_testdata())
  a  <- kw[kw$product_code == "A", ]
  expect_equal(a$cpi_kwartaal[a$kwartaal == "2025K1"], 101)   # (100+101+102)/3
  expect_equal(a$cpi_kwartaal[a$kwartaal == "2025K2"], 104)   # (103+104+105)/3
})

test_that("onvolledig kwartaal valt af", {
  data <- maak_testdata()
  data <- data[!(data$periode == "2025MM12"), ]
  kw <- bereken_kwartaal_cpi(data)
  expect_false("2025K4" %in% kw$kwartaal)
  expect_error(bereken_kwartaalmutatie(data, "2025K4"), "Geen drie maandcijfers")
})

test_that("kwartaalmutatie wordt goed berekend", {
  res <- bereken_kwartaalmutatie(maak_testdata(), "2025K2")
  expect_equal(nrow(res), 2)
  expect_equal(res$mutatie_pct[res$product_code == "A"], 100 * (104 / 101 - 1))
  expect_equal(res$mutatie_pct[res$product_code == "B"], 0)
})

test_that("eerste kwartaal zonder voorganger geeft een duidelijke fout", {
  expect_error(bereken_kwartaalmutatie(maak_testdata(), "2025K1"), "vorige kwartaal")
})

test_that("producten en CPI-kolom worden in de metadata gevonden", {
  cat_meta <- data.frame(Key = c("C1  ", "C2", "C3"),
                         Title = c("011210 Koffie", "011200 Koffie en thee", "045100 Elektriciteit"))
  gevonden <- zoek_categorieen(cat_meta, c("Koffie", "Elektric"))
  expect_equal(gevonden$Key, c("C1", "C3"))
  expect_error(zoek_categorieen(cat_meta, "thee|Koffie"), "Meerdere")

  props <- data.frame(Type = c("Dimension", "Topic", "Topic", "Topic"),
                      Key = c("Perioden", "CPI_1", "CPIAfgeleid_2", "Jaarmutatie_3"),
                      Title = c("Perioden", "CPI", "CPI afgeleid", "Jaarmutatie CPI"))
  expect_equal(zoek_cpi_kolom(props), "CPI_1")
})

test_that("resultaten komen in SQLite terecht", {
  skip_if_not_installed("RSQLite")
  data <- maak_testdata()
  db   <- tempfile(fileext = ".sqlite")
  schrijf_naar_database(db, data, bereken_kwartaal_cpi(data),
                        bereken_alle_kwartaalmutaties(data),
                        bereken_kwartaalmutatie(data, "2025K4"), "TEST")
  expect_equal(nrow(lees_uit_database(db, "cpi_maand")), 24)
  expect_equal(nrow(lees_uit_database(db, "kwartaalmutaties")), 6)
  expect_equal(nrow(lees_uit_database(db, "runlog")), 1)
})
