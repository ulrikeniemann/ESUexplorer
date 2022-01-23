################################################################################
# ESU explorer
#
# maps.R (module, Seite "Karten")
# Ulrike Niemann
#
################################################################################
# Module UI function:
karteErstellenUI <- function(id) {
  ns <- shiny::NS(id)
  # ein umschließendes div - für shinyjs::reset
  div(
    id = ns("form"),
    tagList(
      fluidRow(
        box(
          title = "Analyse auswählen",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          fluidRow(
            # Räumliche Ebene --------------------------------------------------
            column(
              8,
              selectizeInput(ns("raum"),
                label = "Räumliche Gliederung:",
                choices = c(
                  "Mitte gesamt" = "BEZ",
                  "Prognoseräume" = "PRG",
                  "Bezirksregionen" = "BZR",
                  "Planungsräume" = "PLR",
                  "Einschulungsbereiche" = "ESB"
                ),
                selected = "ESB"
              )
            )
          ),
          fluidRow(
            # Analysevariable --------------------------------------------------
            column(
              8,
              selectizeInput(ns("rowVar1"),
                label = "Analysevariable:",
                choices = c(
                  list("Analysevariable auswählen" = ""),
                  varsBereicheKat[-c(1, 2)] # kategoriale, ohne Gesamt/Schuljahr
                ),
                selected = ""
              )
            ),
            column(
              4,
              div(
                class = "missCol",
                checkboxInput(ns("missValuesRowVar1"),
                  label = "fehlende Werte ausschließen",
                  value = FALSE
                )
              )
            )
          ),
          fluidRow(
            column(
              8,
              selectizeInput(ns("level"),
                label = "Ausprägung:",
                choices = c(
                  list("Ausprägung der Analysevariable auswählen" = ""),
                  ""
                ),
                selected = ""
              )
            )
          )
        ),
        # Globale Filter -------------------------------------------------------
        box(
          title = "Globale Filter",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          selectizeInput(ns("jahrFilter"),
            label = "Schuljahr",
            choices = rev(levels(daten[[sym(zeitVar)]])),
            selected = last(levels(daten[[sym(zeitVar)]]))
          )
        ), 
        # Buttons Einstellungen zurücksetzen/speichern/laden -------------------
        column(
          3,
          actionButton(ns("resetSettings"),
            "Einstellungen zurücksetzen",
            icon = icon("undo-alt"),
            width = "100%"
          ),
          actionButton(ns("saveSettings"),
            "Einstellungen speichern",
            icon = icon("file-export"),
            width = "100%"
          ),
          actionButton(ns("loadSettings"),
            "Einstellungen laden",
            icon = icon("file-import"),
            width = "100%"
          )
        )
      ), 
      ##########################################################################
      # Erweiterte Filtereinstellungen
      hidden( # nur für Filter-Code
        checkboxInput(ns("wohnMitte"), label = NULL, value = TRUE),
        selectizeInput(ns("colVar"), label = NULL, choices = "", selected = ""),
        checkboxInput(ns("missValuesColVar1"), label = NULL, value = FALSE),
        selectizeInput(ns("rowVar2"), label = NULL, choices = "", selected = ""),
        checkboxInput(ns("missValuesRowVar2"), label = NULL, value = FALSE)
      ),
      filterDatenUI(id),
      ##########################################################################
      fluidRow(
        box(
          title = "Ergebnisse",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          ### Hinweise versteckt (nur wenn kein Dia) ---------------------------
          hidden(div(
            id = ns("hinweis-zeroData"), class = "hinweis",
            "HINWEIS: Es wird kein Ergebnis erzeugt - keine gültigen Fälle enthalten!"
          )),
          hidden(div(
            id = ns("hinweis-rowVar1"), class = "hinweis",
            "HINWEIS: Bitte wählen Sie eine Analysevariable aus!"
          )),
          hidden(div(
            id = ns("hinweis-level"), class = "hinweis",
            "HINWEIS: Bitte wählen Sie eine Ausprägung der Analysevariable aus!"
          )),
          hidden(div(
            id = ns("hinweis-esb"), class = "hinweis",
            #"HINWEIS: Für die Schuljahre 2010 und 2017
                   #sind keine Analysen nach Einschulungsbereich möglich!"
            "HINWEIS: Für das Schuljahr 2017
                   ist keine Analyse nach Einschulungsbereich möglich!"
          )),
          # Download komplette Analyse -----------------------------------------
          downloadButton(ns("exportAnalyse"),
            label = "Export Excel (alle Tabellen und Grafiken)"
          ),
          br(), br(),
          tabBox(
            height = "auto",
            width = 12,
            # Choroplethenkarte ################################################
            tabPanel(
              "Karte Berlin Mitte",
              ### Grafikbereich ------------------------------------------------
              hidden(div(
                id = ns("plotoutput"),
                div(
                  id = "abbText",
                  htmlOutput(ns("abbText"))
                ),
                withSpinner(ggiraphOutput(ns("plotMapGiraph"),
                  height = "800px"
                )),
                downloadButton(ns("Export"),
                  label = "Export png",
                  class = "exportButton"
                ),
                downloadButton(ns("ExportEdit"),
                  label = "Export xlsx (editierbar)",
                  class = "exportButton"
                )
              )),
              ### erweitere Einstellungen Grafik -------------------------------
              hidden(
                div(
                  id = ns("plotSettings"),
                  box(
                    title = "Erweiterte Einstellungen Grafik",
                    width = 12,
                    status = "primary",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    # Beschriftung teilweise ausblenden?
                    fluidRow(
                      column(
                        3,
                        checkboxInput(ns("valueShow"),
                          label = HTML("Beschriftung:<br>Wert anzeigen"),
                          value = TRUE
                        )
                      ),
                      column(
                        3,
                        checkboxInput(ns("percentHide"),
                          label = "Beschriftung ohne Prozentzeichen %",
                          value = FALSE
                        )
                      ),
                      column(
                        3,
                        selectizeInput(ns("geomTextSize"),
                          label = HTML("Textgröße Werte:"),
                          choices = 12:24,
                          selected = 12
                        )
                      )
                    ),
                    # Skala / Legende
                    fluidRow(
                      column(
                        3,
                        selectizeInput(ns("scale"),
                          label = "Klassenbildung:",
                          choices = c(
                            "Automatische Klassen" = "pretty",
                            "Gleiche Intervalle" = "interval",
                            "Quantile" = "quantile",
                            "Kontinuierliche Skala" = "continuous"
                          ),
                          selected = "pretty"
                        )
                      ),
                      div(
                        id = ns("scaleNumberDiv"),
                        column(
                          3,
                          sliderInput(ns("scaleNumber"),
                            label = "Länge der Intervalle in %:",
                            min = 1, max = 20,
                            value = 5
                          )
                        )
                      ),
                      div(
                        id = ns("quantileNumberDiv"),
                        column(
                          3,
                          sliderInput(ns("quantileNumber"),
                            label = "Anzahl Quantile (ca.):",
                            min = 2, max = 10,
                            value = 5
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        3,
                        selectizeInput(ns("legende"),
                          label = "Postion Legende:",
                          choices = c(
                            "rechts" = "right",
                            "links" = "left",
                            "oben" = "top",
                            "unten" = "bottom"
                          ),
                          selected = "rechts"
                        )
                      ),
                      column(
                        3,
                        selectizeInput(ns("baseSize"),
                          label = HTML("Textgröße Legende:"),
                          choices = 12:24,
                          selected = 12
                        )
                      ),
                      column(
                        3,
                        selectizeInput(ns("color"),
                          label = HTML("Farbpalette:"),
                          choices = c(
                            "Orangetöne" = "OrRd",
                            "Blautöne" = "Blues",
                            "Grüntöne" = "Greens"
                          ),
                          selected = "Orangetöne"
                        )
                      )
                    ),
                    # weitere Anzeigen
                    checkboxInput(ns("path"),
                      label = "Raumgrenzen einblenden",
                      value = TRUE
                    ),
                    checkboxInput(ns("schulen"),
                      label = "Schulen anzeigen",
                      value = FALSE
                    ),
                    checkboxInput(ns("water"),
                      label = "Gewässer einblenden",
                      value = TRUE
                    ),
                    checkboxInput(ns("green"),
                      label = "Grünflächen einblenden",
                      value = FALSE
                    ),
                    checkboxInput(ns("other"),
                      label = "Andere unbewohnte Flächen einblenden",
                      value = FALSE
                    )
                  )
                )
              )
            ),
            # Kernelheaping-Karten #############################################
            # ------------------------------------------------------------------
            tabPanel(
              "Kernelheaping-Karten",
              hidden(div(
                id = ns("kernelheapingCreate"),
                h4(
                  HTML("Hier können Sie Karten nach dem Kernelheaping-Verfahren anfordern.<br>
                       Die Berechnung erfolgt auf Grundlage der aktuell eingestellten Auswertung."),
                  div(
                    class = "hinweis",
                    "ACHTUNG: Die Berechnung nimmt einige Zeit in Anspruch (ca. 1 Minute). 
                    Die Berechnung kann nicht abgebrochen werden 
                    (außer durch Schließen des Browsers / Beenden der Anwendung)."
                  )
                ),
                ### erweitere Einstellungen Grafik -----------------------------
                # doch nicht dem Endnutzer anbieten! (hidden)
                hidden(
                  box(
                    title = "Einstellungen Berechnung Kernelheaping-Karte",
                    width = 12,
                    status = "primary",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    fluidRow(
                      column(
                        3,
                        sliderInput(ns("gridsize"),
                          label = "Rastergröße",
                          min = 100, max = 400,
                          value = 300
                        )
                      ),
                      column(
                        3,
                        sliderInput(ns("burnin"),
                          label = "Burnin",
                          min = 1, max = 5,
                          value = 5
                        )
                      ),
                      column(
                        3,
                        sliderInput(ns("samples"),
                          label = "Samples",
                          min = 5, max = 30,
                          value = 7
                        )
                      )
                    )
                  )
                ), 
                # Kernelheaping Karten anfordern -------------------------------
                actionButton(ns("createKernelMap"),
                  "Kernelheaping-Karten anfordern",
                  icon = icon("play")
                )
              )),
              ### Grafikbereich ------------------------------------------------
              hidden(div(
                id = ns("kernelheapingPlot"),
                div(
                  id = "abbTextK1",
                  verbatimTextOutput(ns("abbTextK1"))
                ),
                withSpinner(ggiraphOutput(ns("plotKernelheaping"),
                  height = "800px"
                )),
                div(
                  id = "abbTextK2",
                  verbatimTextOutput(ns("abbTextK2"))
                ),
                withSpinner(ggiraphOutput(ns("plotKernelheaping2"),
                  height = "800px"
                )),
                downloadButton(ns("ExportKernelheaping"),
                  label = "Export png Kernelheaping",
                  class = "exportButton"
                ),
                downloadButton(ns("ExportKernelheaping2"),
                  label = "Export png Hotspot",
                  class = "exportButton"
                ),
                downloadButton(ns("ExportEditKernelheaping"),
                  label = "Export xlsx (editierbar)",
                  class = "exportButton"
                ),
                actionButton(ns("cancel"),
                  label = "Berechnung zurücksetzen",
                  icon = icon("undo-alt")
                )
              )),
              ### erweitere Einstellungen Grafik -------------------------------
              hidden(div(
                id = ns("kernelheapingPlotSettings"),
                box(
                  title = "Erweiterte Einstellungen Grafik",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  fluidRow(
                    column(
                      3,
                      selectizeInput(ns("legende2"),
                        label = "Postion Legende:",
                        choices = c(
                          "rechts" = "right",
                          "links" = "left",
                          "oben" = "top",
                          "unten" = "bottom"
                        ),
                        selected = "rechts"
                      )
                    ),
                    column(
                      3,
                      selectizeInput(ns("color2"),
                        label = HTML("Farbpalette:"),
                        choices = c(
                          "Orangetöne" = "OrRd",
                          "Blautöne" = "Blues",
                          "Grüntöne" = "Greens"
                        ),
                        selected = "Orangetöne"
                      )
                    )
                  ),
                  # weitere Anzeigen
                  checkboxInput(ns("path2"),
                    label = "Raumgrenzen einblenden",
                    value = TRUE
                  ),
                  checkboxInput(ns("schulen2"),
                    label = "Schulen anzeigen",
                    value = FALSE
                  ),
                  checkboxInput(ns("water2"),
                    label = "Gewässer einblenden",
                    value = TRUE
                  ),
                  checkboxInput(ns("green2"),
                    label = "Grünflächen einblenden",
                    value = FALSE
                  ),
                  checkboxInput(ns("other2"),
                    label = "Andere unbewohnte Flächen einblenden",
                    value = FALSE
                  )
                )
              )),
              ### Hinweise versteckt (nur wenn kein Dia)  ----------------------
              hidden(div(
                id = ns("hinweis-noKernelMap"), class = "hinweis",
                HTML("HINWEIS: Es kann keine Kernelheaping-Karte angefordert werden - nur auf Ebene der Planungsräume oder Einschulungsgebiete!
                     <br>(Unter Räumliche Gliederung auszuwählen.)")
              ))
            ),
            ####################################################################
            # ------------------------------------------------------------------
            tabPanel(
              "Tabellen", "Tabellarische Auswertungen",
              div(
                id = ns("tabOutput"),
                withSpinner(DT::dataTableOutput(ns("propTab"))),
                withSpinner(DT::dataTableOutput(ns("freTab")))
              )
            ),
            # ------------------------------------------------------------------
            tabPanel(
              "Weitere Infos", "Zusätzliche Informationen",
              withSpinner(DT::dataTableOutput(ns("infosRaum"))),
              withSpinner(DT::dataTableOutput(ns("infosRowVar1"))),
              withSpinner(DT::dataTableOutput(ns("filterInfo")))
            )
          ) 
        ) 
      ) 
    ) 
  ) 
}
#
################################################################################
################################################################################
################################################################################
# Module server function:
karteErstellen <- function(input, output, session) {
  ##############################################################################
  ##############################################################################
  # Filter-Code einfügen
  source("scripts/module/filterDataServer.R", encoding = "utf8", local = TRUE)
  # (1) Datenbasis: Daten entsprechend gefiltert -------------------------------
  myDataSet <- reactiveValues(data = NULL)
  # Daten filtern --------------------------------------------------------------
  observe({
    req(input$rowVar1)
    input$filterDo
    myDataSet$data <- dataFilter(
      colVar1 = "",
      missValuesColVar1 = FALSE,
      rowVar1 = input$rowVar1,
      missValuesRowVar1 = input$missValuesRowVar1,
      rowVar2 = "",
      missValuesRowVar2 = FALSE,
      wohnMitte = TRUE,
      jahrFilter = input$jahrFilter,
      filter1 = isolate(filterSet$filter1),
      operator1 = isolate(filterSet$operatorFilter1),
      value1 = isolate(filterSet$valueFilter1),
      filter2 = isolate(filterSet$filter2),
      operator2 = isolate(filterSet$operatorFilter2),
      value2 = isolate(filterSet$valueFilter2),
      filter3 = isolate(filterSet$filter3),
      operator3 = isolate(filterSet$operatorFilter3),
      value3 = isolate(filterSet$valueFilter3)
    )
  }, priority = 10)
  # ----------------------------------------------------------------------------
  # Daten filtern : ohne individuellen Filter (wenn resetFilter geklickt)
  observeEvent(input$resetFilter, {
    # Filter 1 löschen
    updateSelectizeInput(session, "filter1", selected = "")
    updateSelectizeInput(session, "operatorFilter1", selected = "gleich")
    updateSelectizeInput(session, "valueFilter1",
      choices = c("Eine oder mehr Kategorien wählen" = ""),
      selected = ""
    )
    # Filter 2 löschen
    updateSelectizeInput(session, "filter2", selected = "")
    updateSelectizeInput(session, "operatorFilter2", selected = "gleich")
    updateSelectizeInput(session, "valueFilter2",
      choices = c("Eine oder mehr Kategorien wählen" = ""),
      selected = ""
    )
    # Filter 3 löschen
    updateSelectizeInput(session, "filter3", selected = "")
    updateSelectizeInput(session, "operatorFilter3", selected = "gleich")
    updateSelectizeInput(session, "valueFilter3",
      choices = c("Eine oder mehr Kategorien wählen" = ""),
      selected = ""
    )
    # reaktive Werte löschen
    resetFilterVals()
    # Daten ungefiltert neu berechnen
    myDataSet$data <- dataFilter(
      colVar1 = "",
      missValuesColVar1 = FALSE,
      rowVar1 = input$rowVar1,
      missValuesRowVar1 = input$missValuesRowVar1,
      rowVar2 = "",
      missValuesRowVar2 = FALSE,
      wohnMitte = TRUE,
      jahrFilter = input$jahrFilter
    )
  })
  # Levels anzeigen ------------------------------------------------------------
  observe({
    req(myDataSet$data)
    req(input$rowVar1)
    isolate({
      # mögliche Ausprägungen
      choices <- levels(droplevels(myDataSet$data[[input$rowVar1]]))
      # Ausprägungen aktualisieren
      if (levelSet$levelVar == "") {
        # wenn die Ausprägung in den möglichen Ausprägungen liegt, dann beibehalten
        selected <- isolate(input$level)
        if (!(is.null(selected) || selected %in% choices)) {
          selected <- ""
        }
        updateSelectizeInput(session, "level",
          choices = choices,
          selected = selected,
          server = TRUE
        )
      } else {
        # es wurde vorher etwas geladen
        updateSelectizeInput(session, "level",
          choices = choices,
          selected = levelSet$levelVar,
          server = TRUE
        )
        # reactiveValue zurücksetzen
        levelSet$levelVar <- ""
      }
    })
  }, priority = 8)
  ##############################################################################
  # (2) Tabellenbasis für Tabellenoutput ---------------------------------------
  dat <- reactive({
    req(myDataSet$data)
    req(input$rowVar1, input$level, input$raum, input$jahrFilter)
    req(rowsZero() == FALSE)
    req(input$level %in%
      levels(droplevels(myDataSet$data[[isolate(input$rowVar1)]])))
    req(esb10_17() == FALSE)
    if (input$raum == "BEZ") {
      req(input$scale == "pretty")
    }
    createTabData(
      data = myDataSet$data,
      var = input$rowVar1,
      raum = input$raum,
      # level = input$level,
      jahr = input$jahrFilter
    )
  })
  ##############################################################################
  # (3) Tabellenbasis für Plot -------------------------------------------------
  mapData <- reactive({
    req(dat())
    createMapData(dat(), isolate(input$jahrFilter), isolate(input$level))
  })
  ##############################################################################
  ##############################################################################
  # ----------------------------------------------------------------------------
  # reine Fehlerbehandlung:
  # ist überhaupt schon irgendwas passiert? ------------------------------------
  noCalculation <- reactive({
    is.null(myDataSet$data)
  })
  # ----------------------------------------------------------------------------
  # ist zuviel gefiltert? rowsZero == TRUE wenn keine Zeilen mehr im Datensatz
  rowsZero <- reactive({
    rowsZero <- FALSE
    if (noCalculation() == FALSE) {
      rowsZero <- nrow(myDataSet$data) == 0
    }
    return(rowsZero)
  })
  # ----------------------------------------------------------------------------
  # ESB bei Jahr 2017 - wg fehlender Daten nicht möglich (2010 mit shp2011!)
  esb10_17 <- reactive({
    ((input$jahrFilter == "2017") && # || input$jahrFilter == "2010") &&
      input$raum == "ESB")
  })
  # Outputs nicht anzeigen wenn es nichts zum anzeigen gibt --------------------
  observe({
    # keine gültigen Fälle oder keine Berechnungen oder keine Auswahl
    if (rowsZero() == TRUE || noCalculation() == TRUE ||
      input$rowVar1 == "" || input$level == "" ||
      esb10_17() == TRUE
    ) {
      hideElement(id = "plotoutput")
      hideElement(id = "plotSettings")
      hideElement(id = "exportAnalyse")
      hideElement(id = "tabOutput")
      # Kerndichte:
      hideElement(id = "kernelheapingCreate")
      hideElement(id = "kernelheapingPlot")
      # alle Hinweise erst mal zurücksetzen
      hideElement(id = "hinweis-rowVar1")
      hideElement(id = "hinweis-level")
      hideElement(id = "hinweis-zeroData")
      hideElement(id = "hinweis-esb")
    }
    # ESB 2017 - fehlende Daten, nicht möglich
    if (esb10_17() == TRUE) {
      showElement("hinweis-esb")
      hideElement(id = "hinweis-rowVar1")
      hideElement(id = "hinweis-level")
      hideElement(id = "hinweis-zeroData")
      hideElement(id = "hinweis-noKernelMap")
      hideElement(id = "kernelheapingCreate")
    } else {
      hideElement("hinweis-esb")
      # keine Analysevariable ausgewählt
      if (input$rowVar1 == "") {
        showElement(id = "hinweis-rowVar1")
        # Ausprägungen aktualisieren
        updateSelectizeInput(session, "level",
          choices = c("Ausprägung der Analysevariable auswählen" = ""),
          selected = ""
        )
      } else {
        hideElement(id = "hinweis-rowVar1")
        # kein Level der Analysevariable ausgewählt
        if (input$level == "") {
          showElement(id = "hinweis-level")
        } else {
          hideElement(id = "hinweis-level")
        }
      }
    }
    # keine gültigen Fälle aber Auswahl der Variablen
    if (rowsZero() == TRUE && input$rowVar1 != "") {
      showElement(id = "hinweis-zeroData")
      hideElement(id = "hinweis-level")
    } else {
      hideElement(id = "hinweis-zeroData")
    }
    # nur hier alles an Berechnungen ok ! --------------------------------------
    if (rowsZero() == FALSE && noCalculation() == FALSE &&
      (input$rowVar1 != "" && input$level != "") &&
      esb10_17() == FALSE) {
      showElement(id = "plotoutput")
      showElement(id = "plotSettings")
      showElement(id = "exportAnalyse")
      showElement(id = "tabOutput")
      # Kerndichte: abhängig vom Raum
      if (isolate(input$raum == "BEZ" ||
        input$raum == "PRG" ||
        input$raum == "BZR")) {
        showElement(id = "hinweis-noKernelMap")
        hideElement(id = "kernelheapingCreate")
      } else {
        hideElement(id = "hinweis-noKernelMap")
        showElement(id = "kernelheapingCreate")
      }
    }
  }, priority = 3)
  ##############################################################################
  # Fehlerbehandlungen
  # Raum muss immer gewählt sein + keine Skalierung bei Mitte gesamt -----------
  observeEvent(input$raum, {
    # browser()
    if (input$raum == "") {
      reset("raum")
    }
    if (input$raum == "BEZ") {
      # Mitte gesamt nur eine mögliche Skala
      updateSelectizeInput(session, "scale",
        choices = c("Automatische Klassen" = "pretty")
      )
    } else {
      selected <- input$scale
      updateSelectizeInput(session, "scale",
        choices = c(
          "Automatische Klassen" = "pretty",
          "Gleiche Intervalle" = "interval",
          "Quantile" = "quantile",
          "Kontinuierliche Skala" = "continuous"
        ),
        selected = selected
      )
    }
  }, priority = 20)
  # Skala muss immer gewählt sein + abh. Anzeigen-------------------------------
  observeEvent(input$scale, {
    # browser()
    if (input$scale == "") {
      reset("scale")
    }
    if (input$scale == "pretty" || input$scale == "continuous") {
      hideElement(id = "scaleNumberDiv")
      hideElement(id = "quantileNumberDiv")
    } else if (input$scale == "interval") {
      showElement(id = "scaleNumberDiv")
      hideElement(id = "quantileNumberDiv")
    } else if (input$scale == "quantile") {
      showElement(id = "quantileNumberDiv")
      hideElement(id = "scaleNumberDiv")
    }
  })
  # Jahr muss immer gewählt sein -----------------------------------------------
  observeEvent(input$jahrFilter, {
    if (input$jahrFilter == "") {
      reset("jahrFilter")
    }
  }, priority = 20)
  # Legende muss immer gewählt sein --------------------------------------------
  observeEvent(input$legende, {
    if (input$legende == "") {
      reset("legende")
    }
  }, priority = 20)
  observeEvent(input$legende2, {
    if (input$legende2 == "") {
      reset("legende2")
    }
  }, priority = 20)
  # Textgrößen müssen immer gewählt sein ---------------------------------------
  observeEvent(input$geomTextSize, {
    if (input$geomTextSize == "") {
      reset("geomTextSize")
    }
  }, priority = 20)
  observeEvent(input$baseSize, {
    if (input$baseSize == "") {
      reset("baseSize")
    }
  }, priority = 20)
  # Farbe muss immer gewählt sein ----------------------------------------------
  observeEvent(input$color, {
    if (input$color == "") {
      reset("color")
    }
  }, priority = 20)
  observeEvent(input$color2, {
    if (input$color2 == "") {
      reset("color2")
    }
  }, priority = 20)
  ##############################################################################
  # den Text für die Beschriftungen festhalten ---------------------------------
  txtInhalt <- reactive({
    req(input$rowVar1, input$level, input$jahrFilter, input$raum)
    paste0(
      "(Schuljahr: ", input$jahrFilter, "; ", input$raum, ") ",
      varLabels[input$rowVar1], " - ", input$level
    )
  })
  ##############################################################################
  # Tabellenoutput erstellen ---------------------------------------------------
  # Tabellen so wie bei Analysen ausgeben: absolute und relative Häufigkeiten
  tabFre <- reactive({
    req(dat())
    tab <- dat() %>% 
      ungroup() %>% 
      select(-c(zeitVar, "prop", "propTip", "total")) %>%
      spread(1, n, fill = 0, convert = TRUE) %>%
      adorn_totals("row", na.rm = FALSE)
    colnames(tab)[1] <- ""
    return(tab)
  })
  tabPr <- reactive({
    req(dat())
    tab <- dat() %>% 
      ungroup() %>% 
      select(-c(zeitVar, "n", "propTip", "total")) %>%
      spread(1, prop, fill = 0, convert = TRUE) %>%
      adorn_totals("row", na.rm = FALSE)
    tab[, -1] <- lapply(tab[, -1] * 100, sprintf, fmt = "%1.1f%%")
    colnames(tab)[1] <- ""
    return(tab)
  })
  ##############################################################################
  # ggplot erstellen -----------------------------------------------------------
  plot <- reactive({
    req(mapData(), input$scale)
    createGgplot(
      mapData = mapData(),
      valueShow = input$valueShow,
      percentHide = input$percentHide,
      geomTextSize = input$geomTextSize,
      scale = input$scale,
      scaleNumber = input$scaleNumber,
      quantileNumber = input$quantileNumber,
      baseSize = input$baseSize,
      path = input$path,
      schulen = input$schulen,
      legende = input$legende,
      color = input$color,
      water = input$water,
      green = input$green,
      other = input$other
    )
  })
  # ggiraph erstellen ----------------------------------------------------------
  # extra, wegen Export!
  plotGgiraph <- reactive({
    req(plot())
    suppressWarnings(girafe(ggobj = plot()))
  })
  ##############################################################################
  # Info / Metadaten-Tabelle für Variablen erzeugen
  infosRowVar1 <- reactive({
    req(input$rowVar1)
    createInfoTab(input$rowVar1)
  })
  # Variable für Raum
  raumVar <- reactive({
    req(input$raum)
    if (input$raum == "PLR") {
      raumVar <- "FAM_wohn_PLR"
    }
    else if (input$raum == "BZR") {
      raumVar <- "FAM_Wohn_BZR"
    }
    else if (input$raum == "PRG") {
      raumVar <- "FAM_wohn_PRG"
    }
    else if (input$raum == "BEZ") {
      raumVar <- "FAM_Bezirk"
    }
    else {
      raumVar <- input$raum
    } # gilt nur für ESB
  })
  # hier auf die Orginal-Variablen zurückgreifen:
  infosRaum <- reactive({
    req(raumVar())
    createInfoTab(raumVar())
  })
  ##############################################################################
  #### Kernelheaping-Karten
  ##############################################################################
  # Auswertung anfordern: nur bei gültiger Auswertung --------------------------
  # siehe oben observe für normale Karten
  # zurücksetzen / verbergen sobald sich Daten ändern oder CancelButton --------
  observeEvent(c(mapData(), input$cancel), {
    kernelheaping$plot <- NULL
    # abhängig vom Raum: diese Karten nur für PLR und ESB
    if (input$raum == "BEZ" || input$raum == "PRG" || input$raum == "BZR") {
      showElement(id = "hinweis-noKernelMap")
      hideElement(id = "kernelheapingCreate")
      hideElement(id = "kernelheapingPlot")
      hideElement(id = "kernelheapingPlotSettings")
    } else {
      hideElement(id = "hinweis-noKernelMap")
      showElement(id = "kernelheapingCreate")
      hideElement(id = "kernelheapingPlot")
      hideElement(id = "kernelheapingPlotSettings")
    }
  })
  # Kerndichtekarte erzeugen erzeugen: nur mit Button --------------------------
  kernelheaping <- reactiveValues(plotData = NULL, plot = NULL, plot2 = NULL)
  observeEvent(input$createKernelMap, {
    # browser()
    req(mapData())
    kernelheaping$plotData <- withProgress(
      message = "Daten werden berechnet.",
      value = 0, {
        createKernelheapingPlotData(
          mapData = mapData(),
          jahr = input$jahrFilter,
          burnin = input$burnin,
          samples = input$samples,
          gridsize = input$gridsize
        )
      }
    )
  }, priority = 3)
  # wenn kernelheaping$plotData vorliegen ggplot aufbauen ----------------------
  observe({
    req(kernelheaping$plotData)
    # Kerndichtekarte alles
    kernelheaping$plot <- createKernelheapingPlot(
      mapData = isolate(mapData()),
      kData = kernelheaping$plotData,
      hotspot = FALSE,
      legende = input$legende2,
      color = input$color2,
      schulen = input$schulen2,
      path = input$path2,
      water = input$water2,
      green = input$green2,
      other = input$other2
    )
    # Kerndichtekarte Hotspots
    kernelheaping$plot2 <- createKernelheapingPlot(
      mapData = isolate(mapData()),
      kData = kernelheaping$plotData,
      hotspot = TRUE,
      legende = input$legende2,
      color = input$color2,
      schulen = input$schulen2,
      path = input$path2,
      water = input$water2,
      green = input$green2,
      other = input$other2
    )
  }, priority = 2)
  # wenn eine Berechnung vorliegt Grafik zeigen --------------------------------
  observe({
    req(kernelheaping$plot)
    hideElement(id = "kernelheapingCreate")
    showElement(id = "kernelheapingPlot")
    showElement(id = "kernelheapingPlotSettings")
  })
  # ggiraph erstellen ----------------------------------------------------------
  # extra, wegen Export!
  # normale Kernelheaping-Karte
  kernelheapingGgiraph <- reactive({
    req(kernelheaping$plot)
    suppressWarnings(girafe(ggobj = kernelheaping$plot))
  })
  # Hotspot-Kernelheaping-Karte
  kernelheapingGgiraph2 <- reactive({
    req(kernelheaping$plot2)
    suppressWarnings(girafe(ggobj = kernelheaping$plot2))
  })
  ##############################################################################
  ##############################################################################
  #### OUTPUTS
  ##############################################################################
  ##############################################################################
  # tab Karte:
  # Abbildung Caption ----------------------------------------------------------
  output$abbText <- renderText({
    HTML(paste0(
      "Karte: ",
      txtInhalt(),
      "<br><font size='-1'>",
      "(Bei zu wenigen gültigen Fällen (Total < ", smallN,
      ") werden die Ergebnisse für den jeweiligen Raum nicht dargestellt.)",
      "</font>"
    ))
  })
  # Output Plot ----------------------------------------------------------------
  output$plotMapGiraph <- renderggiraph({
    girafe_options(
      plotGgiraph(),
      opts_selection(type = "none"),
      opts_toolbar(saveaspng = FALSE)
    )
  })
  # Download Plot --------------------------------------------------------------
  output$Export <- downloadHandler(
    filename = function() {
      paste0(
        input$rowVar1, "_",
        input$level, "_",
        input$jahrFilter, "_",
        input$raum,
        ".png"
      )
    },
    content = function(file) {
      suppressWarnings(ggsave(file,
        plot = plot(),
        width = 5 * phi, height = 5
      ))
    }
  )
  # Excel (editierbare Vektor-Grafik)
  output$ExportEdit <- downloadHandler(
    filename = function() {
      paste0(
        "Karte_",
        input$rowVar1, "_",
        input$level, "_",
        input$jahrFilter, "_",
        input$raum,
        ".xlsx"
      )
    },
    content = function(file) {
      openxlsxwb <- createWorkbook()
      addWorksheet(openxlsxwb, "Grafik", gridLines = F)
      writeData(openxlsxwb, sheet = "Grafik", txtInhalt())
      suppressWarnings(ggsave("plot.png",
        plot = plot(),
        width = 5 * phi, height = 5
      ))
      insertImage(openxlsxwb, "Grafik", "plot.png",
        width = 5 * phi, height = 5, startRow = 3
      )
      # create a temporary xlsx-file
      tmpwb <- tempfile(fileext = ".xlsx")
      suppressMessages(saveWorkbook(openxlsxwb, tmpwb))
      officerwb <- read_xlsx(tmpwb)
      file.remove(tmpwb)
      # Export plot as editable graphic in xlsx
      officerwb <- add_sheet(officerwb, label = "Grafik (editierbar)")
      officerwb <- xl_add_vg(officerwb,
        sheet = "Grafik (editierbar)",
        code = suppressWarnings(print(plot())),
        width = 5 * phi, height = 5, left = 0, top = 0
      )
      print(officerwb, target = file)
      file.remove("plot.png")
    }
  )
  ##############################################################################
  # Output Tabellen ------------------------------------------------------------
  output$propTab <- renderDT(
    DT::datatable(tabPr(), 
                  caption = paste("Tabelle: ", txtInhalt()),
                  extensions = c("Buttons", "Scroller"),
                  options = list(
                    scrollX = TRUE,
                    scrollY = "400",
                    paging = FALSE,
                    fixedHeader = TRUE,
                    dom = "tB",
                    ordering = FALSE,
                    buttons = c("excel", "print")
                  ),
                  rownames = FALSE,
                  selection = "none"
    )
  )
  output$freTab <- renderDT(
    DT::datatable(tabFre(), 
                  caption = paste("Tabelle: ", txtInhalt()),
                  extensions = c("Buttons", "Scroller"),
                  options = list(
                    scrollX = TRUE,
                    scrollY = "400",
                    paging = FALSE,
                    fixedHeader = TRUE,
                    dom = "tB",
                    ordering = FALSE,
                    buttons = c("excel", "print")
                  ),
                  rownames = FALSE,
                  selection = "none"
    )
  )
  ##############################################################################
  # Weitere Infos (Metadaten)
  output$infosRaum <- renderDT(
    DT::datatable(infosRaum(),
      caption = paste(
        "Meta-Informationen: ",
        varLabels[raumVar()]
      ),
      options = list(
        scrollX = TRUE,
        scrollY = "400",
        paging = FALSE,
        fixedHeader = TRUE,
        dom = "t",
        ordering = FALSE
      ),
      rownames = FALSE, selection = "none"
    )
  )
  output$infosRowVar1 <- renderDT(
    DT::datatable(infosRowVar1(),
      caption = paste(
        "Meta-Informationen: ",
        varLabels[input$rowVar1]
      ),
      options = list(
        scrollX = TRUE,
        scrollY = "400",
        paging = FALSE,
        fixedHeader = TRUE,
        dom = "t",
        ordering = FALSE
      ),
      rownames = FALSE, selection = "none"
    )
  )
  output$filterInfo <- renderDT(
    DT::datatable(filterTab(),
      caption = paste("Aktive Filter: "),
      options = list(
        scrollX = TRUE,
        scrollY = "400",
        paging = FALSE,
        fixedHeader = TRUE,
        dom = "t",
        ordering = FALSE
      ),
      rownames = FALSE, selection = "none"
    )
  )
  ##############################################################################
  # tab Kernelheaping-Karten ---------------------------------------------------
  # Abbildung Caption ----------------------------------------------------------
  output$abbTextK1 <- renderText({
    paste("Karte nach Kernelheaping-Verfahren: ", txtInhalt())
  })
  output$abbTextK2 <- renderText({
    paste(
      "Hotspot-Karte nach Kernelheaping-Verfahren: 
      Die", (1 - hotspotQuantile) * 100, "%",
      "der Fläche Berlin-Mittes mit den höchsten Anteilswerten: 
      ", txtInhalt()
    )
  })
  # Output Kernelheaping Map ---------------------------------------------------
  output$plotKernelheaping <- renderggiraph({
    girafe_options(
      kernelheapingGgiraph(),
      opts_selection(type = "none"),
      opts_toolbar(saveaspng = FALSE)
    )
  })
  # Output Hotspot-Kernelheaping Map -------------------------------------------
  output$plotKernelheaping2 <- suppressWarnings(renderggiraph({
    girafe_options(
      kernelheapingGgiraph2(),
      opts_selection(type = "none"),
      opts_toolbar(saveaspng = FALSE)
    )
  }))
  # Download Kernelheaping Map -------------------------------------------------
  # normale Kernelheaping-Karte
  output$ExportKernelheaping <- downloadHandler(
    filename = function() {
      paste0(
        "KarteKernelheaping_",
        input$rowVar1, "_",
        input$level, "_",
        input$jahrFilter, "_",
        input$raum,
        ".png"
      )
    },
    content = function(file) {
      suppressWarnings(ggsave(file,
        plot = kernelheaping$plot,
        width = 5 * phi, height = 5
      ))
    }
  )
  # Hotspot-Kernelheaping-Karte
  output$ExportKernelheaping2 <- downloadHandler(
    filename = function() {
      paste0(
        "KarteKernelheapingHotspot_",
        input$rowVar1, "_",
        input$level, "_",
        input$jahrFilter, "_",
        input$raum,
        ".png"
      )
    },
    content = function(file) {
      suppressWarnings(ggsave(file,
        plot = kernelheaping$plot2,
        width = 5 * phi, height = 5
      ))
    }
  )
  # Excel (editierbare Vektor-Grafik)
  output$ExportEditKernelheaping <- downloadHandler(
    filename = function() {
      paste0(
        "KartenKernelheaping_",
        input$rowVar1, "_",
        input$level,
        input$jahrFilter, "_",
        input$raum,
        ".xlsx"
      )
    },
    content = function(file) {
      openxlsxwb <- createWorkbook()
      # normale Kernelheaping-Karte
      addWorksheet(openxlsxwb, "Grafik", gridLines = F)
      writeData(openxlsxwb,
        sheet = "Grafik",
        paste(
          "Karte nach Kernelheaping-Verfahren: ",
          txtInhalt()
        )
      )
      suppressWarnings(ggsave("plot2.png",
        plot = kernelheaping$plot,
        width = 5 * phi, height = 5
      ))
      insertImage(openxlsxwb, "Grafik", "plot2.png",
        width = 5 * phi, height = 5, startRow = 3
      )
      # Hotspot-Kernelheaping-Karte
      addWorksheet(openxlsxwb, "HotspotGrafik", gridLines = F)
      writeData(openxlsxwb,
        sheet = "HotspotGrafik",
        paste(
          "Hotspot-Karte nach Kernelheaping-Verfahren: 
                Die", (1 - hotspotQuantile) * 100, "%",
          "der Fläche Berlin-Mittes mit den höchsten Anteilswerten: 
                ", txtInhalt()
        )
      )
      suppressWarnings(ggsave("plot3.png",
        plot = kernelheaping$plot2,
        width = 5 * phi, height = 5
      ))
      insertImage(openxlsxwb, "HotspotGrafik", "plot3.png",
        width = 5 * phi, height = 5, startRow = 3
      )
      # create a temporary xlsx-file
      tmpwb <- tempfile(fileext = ".xlsx")
      suppressMessages(saveWorkbook(openxlsxwb, tmpwb))
      officerwb <- read_xlsx(tmpwb)
      file.remove(tmpwb)
      # Export plot as editable graphic in xlsx
      # normale Kernelheaping-Karte
      officerwb <- add_sheet(officerwb, label = "Grafik (editierbar)")
      officerwb <- xl_add_vg(officerwb,
        sheet = "Grafik (editierbar)",
        code = suppressWarnings(print(kernelheaping$plot)),
        width = 5 * phi, height = 5, left = 0, top = 0
      )
      # Hotspot-Kernelheaping-Karte
      officerwb <- add_sheet(officerwb, label = "HotspotGrafik (editierbar)")
      officerwb <- xl_add_vg(officerwb,
        sheet = "HotspotGrafik (editierbar)",
        code = suppressWarnings(print(kernelheaping$plot2)),
        width = 5 * phi, height = 5, left = 0, top = 0
      )
      print(officerwb, target = file)
      if (file.exists("plot2.png")) {
        file.remove("plot2.png")
      }
      if (file.exists("plot3.png")) {
        file.remove("plot3.png")
      }
    }
  )
  ##############################################################################
  # Export komplette Analyse ---------------------------------------------------
  output$exportAnalyse <- downloadHandler(
    filename = function() {
      paste0(
        "Analyse_",
        input$rowVar1, "_",
        input$level, "_",
        input$jahrFilter, "_",
        input$raum,
        ".xlsx"
      )
    },
    content = function(file) {
      wb <- createWorkbook()
      # sheet Prozente ---------------------------------------------------------
      addWorksheet(wb, "Prozente")
      writeData(wb,
                sheet = "Prozente",
                paste("Tabelle Relative Häufigkeiten:", txtInhalt())
      )
      writeData(wb, sheet = "Prozente", tabPr(), startRow = 3)
      addStyle(wb,
               sheet = "Prozente",
               style = pctStyle, # pctStyle siehe Konstanten
               cols = 2:ncol(tabPr()),
               rows = 4:(nrow(tabPr()) + 3),
               gridExpand = TRUE
      )
      # sheet Häufigkeiten -----------------------------------------------------
      addWorksheet(wb, "Häufigkeiten")
      writeData(wb,
                sheet = "Häufigkeiten",
                paste("Tabelle Absolute Häufigkeiten:", txtInhalt())
      )
      writeData(wb, sheet = "Häufigkeiten", tabFre(), startRow = 3)
      # sheet Metadaten --------------------------------------------------------
      addWorksheet(wb, "Metadaten")
      writeData(wb, sheet = "Metadaten", paste("Metadaten:", txtInhalt()))
      writeData(wb, sheet = "Metadaten", infosRaum(), startRow = 3)
      writeData(wb, sheet = "Metadaten", infosRowVar1(), startRow = 10)
      writeData(wb, sheet = "Metadaten", "Filterinformationen:", startRow = 17)
      writeData(wb, sheet = "Metadaten", filterTab(), startRow = 18)
      # Karte ------------------------------------------------------------------
      addWorksheet(wb, "Karte", gridLines = F)
      writeData(wb, sheet = "Karte", paste("Karte:", txtInhalt()))
      suppressWarnings(ggsave("plot.png", plot = plot(),
                              width = 5 * phi, height = 5))
      insertImage(wb, "Karte", "plot.png",
                  width = 5 * phi, height = 5, startRow = 3)
      # Kernelheaping-Karte ----------------------------------------------------
      if (!is.null(kernelheaping$plot)) {
        addWorksheet(wb, "Kernelheaping", gridLines = F)
        writeData(wb,
          sheet = "Kernelheaping",
          paste("Karte nach Kernelheaping-Verfahren:", txtInhalt())
        )
        suppressWarnings(ggsave("plot2.png",
          plot = kernelheaping$plot,
          width = 5 * phi, height = 5))
        insertImage(wb, "Kernelheaping", "plot2.png",
                    width = 5 * phi, height = 5, startRow = 3)
      }
      # Hotspot-Kernelheaping-Karte --------------------------------------------
      if (!is.null(kernelheaping$plot2)) {
        addWorksheet(wb, "Hotspot", gridLines = F)
        writeData(wb,
          sheet = "Hotspot",
          paste(
            "Hotspot-Karte nach Kernelheaping-Verfahren: 
                Die", (1 - hotspotQuantile) * 100, "%",
            "der Fläche Berlin-Mittes mit den höchsten Anteilswerten: 
                ", txtInhalt()
          )
        )
        suppressWarnings(ggsave("plot3.png",
          plot = kernelheaping$plot2,
          width = 5 * phi, height = 5))
        insertImage(wb, "Hotspot", "plot3.png",
          width = 5 * phi, height = 5, startRow = 3)
      }
      # editierbare Grafiken ---------------------------------------------------
      # create a temporary xlsx-file
      tmpwb <- tempfile(fileext = ".xlsx")
      suppressMessages(saveWorkbook(wb, tmpwb))
      officerwb <- read_xlsx(tmpwb)
      file.remove(tmpwb)
      # Export plot as editable graphic in xlsx
      officerwb <- add_sheet(officerwb, label = "Karte (editierbar)")
      officerwb <- xl_add_vg(officerwb,
        sheet = "Karte (editierbar)",
        code = suppressWarnings(print(plot())),
        width = 5 * phi, height = 5, left = 0, top = 0)
      # editierbare Kernelheaping-Karte
      if (!is.null(kernelheaping$plot)) {
        officerwb <- add_sheet(officerwb, label = "Kernelheaping (editierbar)")
        officerwb <- xl_add_vg(officerwb,
          sheet = "Kernelheaping (editierbar)",
          code = suppressWarnings(print(kernelheaping$plot)),
          width = 5 * phi, height = 5, left = 0, top = 0)
      }
      # editierbare Hotspot-Kernelheaping-Karte
      if (!is.null(kernelheaping$plot2)) {
        officerwb <- add_sheet(officerwb, label = "Hotspot (editierbar)")
        officerwb <- xl_add_vg(officerwb,
          sheet = "Hotspot (editierbar)",
          code = suppressWarnings(print(kernelheaping$plot2)),
          width = 5 * phi, height = 5, left = 0, top = 0)
      }
      # ------------------------------------------------------------------------
      print(officerwb, target = file)
      # ------------------------------------------------------------------------
      if (file.exists("plot.png")) {
        file.remove("plot.png")
      }
      if (file.exists("plot2.png")) {
        file.remove("plot2.png")
      }
      if (file.exists("plot3.png")) {
        file.remove("plot3.png")
      }
    }
  )
  ##############################################################################
  # Einstellungen zurücksetzen/speichern/laden
  ##############################################################################
  source("scripts/module/saveLoadResetServer.R", encoding = "utf8", local = TRUE)
  ##############################################################################
}
################################################################################
