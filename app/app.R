# =============================================================================
# app/app.R  -  Shiny-app bovenop het R-proces
# De app rekent zelf niets uit: hij leest de SQLite-database die run.R vult
# en kan het proces opnieuw starten. Zo blijft er een bron van waarheid.
#
# Starten vanuit de projectmap:  shiny::runApp("app")
# =============================================================================
library(shiny)

projectmap <- normalizePath("..")
source(file.path(projectmap, "run.R"), chdir = TRUE, encoding = "UTF-8")
db <- Sys.getenv("CPI_DB", CONFIG$db)
# Een relatief SQLite-pad geldt vanaf de projectmap; een ODBC-string blijft zoals hij is.
if (!is_odbc(db) && !grepl("^([A-Za-z]:)?[/\\\\]", db)) db <- file.path(projectmap, db)

#' Is de database bereikbaar en gevuld?
database_klaar <- function(db) {
  if (!is_odbc(db) && !file.exists(db)) return(FALSE)
  isTRUE(tryCatch(tabel_bestaat(db, "kwartaalmutaties"), error = function(e) FALSE))
}

ui <- fluidPage(
  titlePanel("Kwartaalmutatie CPI"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("verslagperiode", "Verslagperiode", choices = NULL),
      checkboxGroupInput("producten", "Producten", choices = NULL),
      hr(),
      checkboxInput("met_sc", "Met seizoencorrectie (JDemetra+)", value = isTRUE(CONFIG$seizoencorrectie)),
      actionButton("verversen", "Data verversen uit StatLine", icon = icon("rotate")),
      helpText("Haalt de nieuwste cijfers op, rekent opnieuw en vult de database.",
               "Zonder vinkje worden eerdere seizoengecorrigeerde cijfers verwijderd."),
      hr(),
      downloadButton("download", "Download mutaties (csv)")
    ),
    mainPanel(
      width = 9,
      h4(textOutput("kop")),
      tableOutput("verslagtabel"),
      tabsetPanel(
        tabPanel("Kwartaalmutatie", plotOutput("grafiek_mutatie", height = 380)),
        tabPanel("CPI per maand",  plotOutput("grafiek_maand",   height = 380)),
        tabPanel("Ruw vs. seizoengecorrigeerd", plotOutput("grafiek_sc", height = 520)),
        tabPanel("Runlog",         tableOutput("runlog"))
      )
    )
  )
)

server <- function(input, output, session) {
  ververs_teller <- reactiveVal(0)

  # Gegevens uit de database; worden opnieuw gelezen na elke verversing.
  gegevens <- reactive({
    ververs_teller()
    validate(need(database_klaar(db),
                  paste("Nog geen resultaten in de database (", db_omschrijving(db), ").",
                        "Draai eerst draai_proces() of klik op 'Data verversen'.")))
    list(maand    = lees_uit_database(db, "cpi_maand"),
         mutaties = lees_uit_database(db, "kwartaalmutaties"),
         runlog   = lees_uit_database(db, "runlog"),
         # Optioneel: alleen aanwezig als run.R met --sc is gedraaid.
         mutaties_sc = if (tabel_bestaat(db, "kwartaalmutaties_sc"))
           lees_uit_database(db, "kwartaalmutaties_sc") else NULL)
  })

  # Vinkje bij het starten: aan als de configuratie het zegt of de database al gecorrigeerde cijfers heeft.
  observeEvent(gegevens(), once = TRUE, {
    updateCheckboxInput(session, "met_sc",
                        value = isTRUE(CONFIG$seizoencorrectie) || !is.null(gegevens()$mutaties_sc))
  })

  observeEvent(gegevens(), {
    periodes <- sort(unique(gegevens()$mutaties$verslagperiode), decreasing = TRUE)
    producten <- sort(unique(gegevens()$maand$product))
    updateSelectInput(session, "verslagperiode", choices = periodes, selected = periodes[1])
    updateCheckboxGroupInput(session, "producten", choices = producten, selected = producten)
  })

  observeEvent(input$verversen, {
    id <- showNotification("Bezig met ophalen en rekenen...", duration = NULL)
    on.exit(removeNotification(id), add = TRUE)
    resultaat <- tryCatch({
      oud <- setwd(projectmap); on.exit(setwd(oud), add = TRUE)
      draai_proces(seizoencorrectie = isTRUE(input$met_sc))
      "ok"
    }, error = function(e) conditionMessage(e))
    if (identical(resultaat, "ok")) {
      showNotification("Data ververst.", type = "message")
      ververs_teller(ververs_teller() + 1)
    } else {
      showNotification(paste("Verversen mislukt:", resultaat), type = "error", duration = 10)
    }
  })

  gekozen_mutaties <- reactive({
    m <- gegevens()$mutaties
    m[m$product %in% input$producten, ]
  })

  output$kop <- renderText({
    req(input$verslagperiode)
    paste("Kwartaalmutatie", input$verslagperiode, "t.o.v.", vorig_kwartaal(input$verslagperiode))
  })

  output$verslagtabel <- renderTable({
    req(input$verslagperiode)
    m <- gekozen_mutaties()
    m <- m[m$verslagperiode == input$verslagperiode, ]
    tabel <- data.frame(Product = m$product,
               `CPI kwartaal` = sprintf("%.2f", m$cpi_kwartaal),
               `CPI vorig kwartaal` = sprintf("%.2f", m$cpi_vorig_kwartaal),
               `Mutatie (%)` = sprintf("%+.1f", m$mutatie_pct),
               check.names = FALSE)
    sc <- gegevens()$mutaties_sc
    if (!is.null(sc)) {
      sc <- sc[sc$verslagperiode == input$verslagperiode, ]
      tabel$`Mutatie seizoengecorrigeerd (%)` <-
        sprintf("%+.1f", sc$mutatie_pct[match(m$product_code, sc$product_code)])
    }
    tabel
  })

  output$grafiek_mutatie <- renderPlot({
    req(nrow(gekozen_mutaties()) > 0)
    grafiek_kwartaalmutatie(gekozen_mutaties())
  }, res = 110)

  output$grafiek_maand <- renderPlot({
    m <- gegevens()$maand
    m <- m[m$product %in% input$producten, ]
    req(nrow(m) > 0)
    grafiek_maand_cpi(m)
  }, res = 110)

  output$grafiek_sc <- renderPlot({
    sc <- gegevens()$mutaties_sc
    validate(need(!is.null(sc),
                  paste("Geen seizoengecorrigeerde cijfers in de database.",
                        "Vink 'Met seizoencorrectie' aan en klik op 'Data verversen',",
                        "of draai draai_proces(seizoencorrectie = TRUE) en start de app opnieuw.")))
    ruw <- gekozen_mutaties()
    sc  <- sc[sc$product %in% input$producten, ]
    req(nrow(ruw) > 0)
    grafiek_ruw_vs_sc(rbind(transform(ruw, reeks = "Ruw"),
                            transform(sc,  reeks = "Seizoengecorrigeerd")))
  }, res = 110)

  output$runlog <- renderTable({
    r <- gegevens()$runlog
    r[order(r$tijdstip, decreasing = TRUE), ]
  })

  output$download <- downloadHandler(
    filename = function() paste0("kwartaalmutaties_", Sys.Date(), ".csv"),
    content  = function(bestand) write.csv2(gekozen_mutaties(), bestand, row.names = FALSE)
  )
}

shinyApp(ui, server)
