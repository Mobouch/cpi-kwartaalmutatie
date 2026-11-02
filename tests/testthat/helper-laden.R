# Laadt de projectfuncties en maakt kleine testdata (fictief, alleen voor tests).
projectmap <- normalizePath(file.path("..", ".."))
for (bestand in sort(list.files(file.path(projectmap, "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(bestand, encoding = "UTF-8")
}

maak_testdata <- function() {
  maanden <- expand.grid(maand = 1:12, jaar = 2025, product_code = c("A", "B"),
                         stringsAsFactors = FALSE)
  maanden$product  <- ifelse(maanden$product_code == "A", "Product A", "Product B")
  maanden$periode  <- sprintf("%dMM%02d", maanden$jaar, maanden$maand)
  maanden$kwartaal <- sprintf("%dK%d", maanden$jaar, (maanden$maand - 1) %/% 3 + 1)
  # Product A stijgt elke maand 1 punt vanaf 100; product B is constant 100.
  maanden$cpi <- ifelse(maanden$product_code == "A", 99 + maanden$maand, 100)
  maanden[, c("product_code", "product", "periode", "jaar", "maand", "kwartaal", "cpi")]
}
