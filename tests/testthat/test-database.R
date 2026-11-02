test_that("SQLite-pad en ODBC-string worden herkend", {
  expect_false(is_odbc("output/cpi.sqlite"))
  expect_true(is_odbc("DRIVER={SQL Server};SERVER=server\\i01,50001;DATABASE=db;"))
  expect_equal(db_pakket("output/cpi.sqlite"), "RSQLite")
  expect_equal(db_pakket("DRIVER={SQL Server};SERVER=s;DATABASE=d;"), "odbc")
})

test_that("de omschrijving toont geen servernaam", {
  oms <- db_omschrijving("DRIVER={SQL Server};SERVER=geheime_server\\i01,50001;DATABASE=db;")
  expect_equal(oms, "SQL Server (ODBC)")
  expect_false(grepl("geheime_server", oms))
})

test_that("tabelnaam gebruikt het voorvoegsel", {
  expect_equal(tabelnaam("runlog", prefix = ""), "runlog")
  expect_equal(tabelnaam("runlog", prefix = "casus_cpi_"), "casus_cpi_runlog")
})

test_that("schrijven en teruglezen werkt, ook met voorvoegsel en SQL-query", {
  skip_if_not_installed("RSQLite")
  oud <- CONFIG$tabel_prefix
  on.exit(CONFIG$tabel_prefix <<- oud, add = TRUE)
  CONFIG$tabel_prefix <<- "casus_cpi_"

  data <- maak_testdata()
  db   <- tempfile(fileext = ".sqlite")
  schrijf_naar_database(db, data, bereken_kwartaal_cpi(data),
                        bereken_alle_kwartaalmutaties(data),
                        bereken_kwartaalmutatie(data, "2025K4"), "TEST")

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  tabellen <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  expect_true(all(c("casus_cpi_cpi_maand", "casus_cpi_kwartaalmutaties", "casus_cpi_runlog") %in% tabellen))
  expect_true(tabel_bestaat(db, "kwartaalmutaties"))

  res <- lees_mutaties(db, "2025K2")
  expect_equal(nrow(res), 2)
  expect_equal(res$mutatie_pct[res$product_code == "A"], 100 * (104 / 101 - 1))
  expect_error(lees_mutaties(db, "2025Q2"), "JJJJKn")
})
