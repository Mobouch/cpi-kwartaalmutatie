# Kwartaalmutatie CPI uit StatLine

Een klein, op zichzelf staand R-proces dat de **kwartaalmutatie van de consumentenprijsindex (CPI)** berekent voor twee producten: **koffie** en **elektriciteit**. De cijfers komen rechtstreeks uit CBS StatLine. Er worden geen csv- of Excel-bestanden gebruikt.

Het proces is opgedeeld in drie duidelijke stappen:

| Stap | Bestand | Wat gebeurt er |
| --- | --- | --- |
| Input | `R/01_input.R` | CPI-maandcijfers ophalen uit StatLine-tabel 86141NED via de CBS Open Data-API (`cbsodataR`) |
| Throughput | `R/02_throughput.R` | Kwartaal-CPI = gemiddelde van drie maanden; kwartaalmutatie t.o.v. vorig kwartaal |
| Output | `R/03_output.R` | Grafieken (PNG) en resultaten in een lokale SQLite-database |
| Optioneel | `R/04_seizoencorrectie.R` | Seizoengecorrigeerde variant met JDemetra+ (zie hieronder) |

`run.R` voert de drie stappen achter elkaar uit. De Shiny-app in `app/` toont de resultaten uit de database en kan het proces opnieuw starten.

## Bron

- **Tabel:** [86141NED – Consumentenprijzen; CPI 2025=100, index en mutaties](https://www.cbs.nl/nl-nl/cijfers/detail/86141NED). Dit is de CPI-tabel vanaf 2026; de oude tabel 83131NED (2015=100) loopt tot en met 2025.
- **Meetwaarde:** CPI-indexcijfer (2025=100), maandcijfers.
- **Producten:** worden op naam gezocht in de metadata van de tabel (`R/00_config.R`). Zo hoeven de interne StatLine-codes niet in de code te staan. Vindt de code nul of meerdere producten, dan stopt hij met een duidelijke melding.

## Berekening

1. **Kwartaal-CPI** = gemiddelde van de drie maand-CPI's in dat kwartaal. Kwartalen met minder dan drie maandcijfers tellen niet mee.
2. **Kwartaalmutatie (%)** = (CPI kwartaal / CPI vorig kwartaal − 1) × 100.

De berekening zit in de functie `bereken_kwartaalmutatie(maanddata, verslagperiode)`, met de verslagperiode als parameter in de vorm `"2026K3"`. De jaargrens wordt goed afgehandeld (2026K1 wordt vergeleken met 2025K4). De database bevat de onafgeronde waarden; afronden gebeurt pas bij het tonen.

## Gebruik

Vereisten: R 4.1 of hoger en de pakketten `cbsodataR`, `dplyr`, `ggplot2`, `DBI` en `RSQLite` (of `odbc` voor SQL Server). Voor de app is daarnaast `shiny` nodig en voor de tests `testthat`.

```r
install.packages(c("cbsodataR", "dplyr", "ggplot2", "DBI", "RSQLite", "shiny", "testthat"))
```

Pak de zip eerst uit naar een eigen map. Open daarna in RStudio het bestand `cpi-kwartaalmutatie.Rproj`; de werkmap staat dan goed. `run.R` zoekt de projectmap ook zelf als je hem vanuit een andere map start.

Vanuit de projectmap:

```bash
Rscript run.R            # laatste volledige kwartaal
Rscript run.R 2026K2     # een gekozen verslagperiode
```

Of in R:

```r
source("run.R")
draai_proces("2026K3")
shiny::runApp("app")     # de Shiny-app
```

## Database: SQLite of SQL Server

Standaard schrijft het proces naar een lokaal SQLite-bestand (`output/cpi.sqlite`). Daarvoor is alleen het R-pakket `RSQLite` nodig; SQLite zelf hoeft niet apart geïnstalleerd te worden.

Het proces kan ook naar SQL Server schrijven en daaruit lezen, via ODBC (pakket `odbc`):

1. Kopieer `lokaal_voorbeeld.R` naar `lokaal.R`.
2. Zet daarin je ODBC-verbindingsstring in `CONFIG$db` en kies een voorvoegsel voor de tabellen, bijvoorbeeld `casus_cpi_`.
3. Draai het proces zoals altijd. `run.R` en de app laden `lokaal.R` automatisch.

`lokaal.R` staat in `.gitignore`, zodat servernamen en andere verbindingsgegevens nooit in Git komen. In de console en de runlog verschijnt alleen "SQL Server (ODBC)", niet de servernaam.

Teruglezen kan per tabel met `lees_uit_database(CONFIG$db, "kwartaalmutaties")` of met een SQL-query per verslagperiode:

```r
lees_mutaties(CONFIG$db, "2026K3")          # ongecorrigeerd
lees_mutaties(CONFIG$db, "2026K3", TRUE)    # seizoengecorrigeerd
```

De query gebruikt een parameter in plaats van tekst in de SQL te plakken, en werkt daardoor op SQLite en SQL Server op dezelfde manier.

## Optioneel: seizoencorrectie

De casus vraagt om de kwartaalmutatie van de **ruwe** CPI, en dat is de standaard. Een kwartaalgemiddelde haalt het seizoenpatroon echter niet weg. Een kwartaal- of maandmutatie van de ruwe cijfers bevat dus ook seizoeninvloeden. Daarom kan het proces daarnaast een seizoengecorrigeerde variant maken (`R/04_seizoencorrectie.R`).

- **Methode:** X13 in JDemetra+ via `rjd3x13`, met specificatie RSA3: automatische keuze log/niveau, uitbijters en ARIMA-model, zonder werkdag- of paascorrectie, want die zijn voor prijzen niet zinvol. Is `rjd3x13` er niet, dan gebruikt het proces `RJDemetra` (JDemetra+ 2). Welk pakket wordt gebruikt, stel je in met `sc_pakket` in `R/00_config.R`: `"auto"`, `"rjd3x13"` of `"RJDemetra"`.
- **Waarom JDemetra+:** het is binnen het Europees Statistisch Systeem de aanbevolen software voor seizoencorrectie en sluit aan op de CBS-productie. De volledige onderbouwing, met de alternatieven die bewust niet gekozen zijn, staat bovenin `R/04_seizoencorrectie.R`.
- **Werkwijze:** eerst de maandreeks corrigeren, daarna met dezelfde functies het kwartaalgemiddelde en de mutatie berekenen. Ook de maand-op-maandmutatie is beschikbaar via `bereken_maandmutatie()`.
- **Controles:** de reeks moet minstens 3 jaar lang zijn, zonder gaten of lege waarden.
- **Let op revisies:** bij elke nieuwe maand wordt de hele reeks opnieuw gecorrigeerd. De laatste gecorrigeerde cijfers kunnen dus nog veranderen.

```r
install.packages("rjd3x13")   # vraagt Java 21 of hoger
```

```bash
Rscript run.R --sc           # ruw + seizoengecorrigeerd
Rscript run.R 2026K2 --sc
```

Met `--sc` komen er twee tabellen bij in de database (`cpi_maand_sc`, `kwartaalmutaties_sc`), een extra grafiek `kwartaalmutatie_ruw_vs_sc.png` en een extra tabblad in de Shiny-app. In de app zet je de seizoencorrectie aan of uit met het vinkje "Met seizoencorrectie" voordat je op "Data verversen" klikt. Let op: een run zonder seizoencorrectie verwijdert de eerdere gecorrigeerde tabellen, zodat er geen oude gecorrigeerde cijfers naast nieuwe ongecorrigeerde cijfers staan. De tests gebruiken de klassieke decompositie uit basis-R (`methode = "decompose"`), zodat ze ook zonder Java draaien. Die methode is alleen bedoeld als controle, niet voor productie.

## Uitvoer

In de map `output/` (staat niet in git):

- `cpi_per_maand.png` – maandelijkse CPI per product
- `kwartaalmutatie.png` – kwartaalmutaties van de laatste 12 kwartalen
- `cpi.sqlite` – database met de tabellen:
  - `cpi_maand` – de opgehaalde maandcijfers
  - `cpi_kwartaal` – de kwartaal-CPI's
  - `kwartaalmutaties` – de mutaties voor alle kwartalen
  - `runlog` – per run het tijdstip, de bron, de verslagperiode en het aantal records

De tabellen worden bij elke run helemaal opnieuw gevuld. Een run is daardoor herhaalbaar: twee keer draaien geeft hetzelfde resultaat.

## Kwaliteit

- **Tests:** `testthat::test_dir("tests/testthat")` controleert de berekening met kleine, zelfgemaakte testdata: het kwartaalgemiddelde, de mutatie, de jaargrens, onvolledige kwartalen, het zoeken van producten, het wegschrijven naar SQLite en de seizoencorrectie (42 controles).
- **Foutafhandeling:** duidelijke meldingen bij een ontbrekend pakket, een onbekend product, een verkeerde verslagperiode of een onvolledig kwartaal.
- **Instellingen op één plek:** `R/00_config.R`. Wil je andere producten? Pas `producten` aan.
- **Reproduceerbaarheid:** gebruik eventueel `renv::init()` om de pakketversies vast te leggen.

## Structuur

```
├── R/
│   ├── 00_config.R       instellingen en pakketcontrole
│   ├── 01_input.R        INPUT
│   ├── 02_throughput.R   THROUGHPUT
│   ├── 03_output.R       OUTPUT
│   └── 04_seizoencorrectie.R  optioneel: seizoencorrectie
├── app/app.R             Shiny-app
├── tests/testthat/       unittests
├── run.R                 draait het hele proces
├── lokaal_voorbeeld.R    voorbeeld voor lokale instellingen (lokaal.R staat niet in Git)
├── cpi-kwartaalmutatie.Rproj  RStudio-project (zet de werkmap goed)
└── README.md
```
