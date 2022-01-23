################################################################################
# ESU explorer
#
# analysen.R (module, Seiten "Zeitreihen Analyse", "Jährliche Analyse", "Individuelle Analyse")
# Ulrike Niemann
#
################################################################################
# Module UI function:
analysenUI <- function(id) {
  ns <- shiny::NS(id)
  # ein umschließendes div - für shinyjs::reset
  div(
    id = ns("form"),
    tagList(
      fluidRow(
        box(
          title = "Analysevariablen auswählen",
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          # ZEILENVARIABLE -----------------------------------------------------
          fluidRow(
            column(
              8,
              selectizeInput(
                ns("rowVar1"),
                label = "Analysevariable:",
                choices = c(
                  list("Analysevariable auswählen" = ""),
                  varsBereiche[-c(1, 2)] # ohne Gesamt/Schuljahr
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
          # SPALTENVARIABLE ----------------------------------------------------
          div(
            id = ns("colVarDiv"),
            fluidRow(
              column(
                8,
                selectizeInput(
                  ns("colVar1"),
                  label = "Spaltenvariable:",
                  choices = c(
                    list("Spaltenvariable auswählen" = ""),
                    varsBereiche
                  ),
                  selected = "Gesamt"
                )
              ),
              column(
                4,
                div(
                  class = "missCol",
                  checkboxInput(
                    ns("missValuesColVar1"),
                    label = "fehlende Werte ausschließen",
                    value = TRUE
                  )
                )
              )
            )
          ),
          # UNTERTEILUNGSVARIABLE ----------------------------------------------
          fluidRow(
            column(
              8,
              selectizeInput(
                ns("rowVar2"),
                label = "Weitere Unterteilung der Auswertung (optional):",
                choices = c(
                  list("Weitere Unterteilung auswählen" = ""),
                  varsBereicheKat[-1] # kategoriale Vars ohne Gesamt
                ),
                selected = ""
              )
            ),
            column(
              4,
              div(
                class = "missCol",
                checkboxInput(ns("missValuesRowVar2"),
                  label = "fehlende Werte ausschließen",
                  value = TRUE
                )
              )
            )
          )
        ), # Ende box
        # Globale Filter -------------------------------------------------------
        box(
          title = "Globale Filter",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          checkboxInput(ns("wohnMitte"),
            label = "nur Wohnort Berlin Mitte",
            value = TRUE
          ),
          selectizeInput(ns("jahrFilter"),
            label = "Schuljahr:",
            choices = c(
              "Alle Jahre" = "Alle Jahre",
              rev(levels(daten[[sym(zeitVar)]]))
            ),
            selected = "Alle Jahre"
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
          hidden(actionButton(ns("loadSettings"),
            "Einstellungen laden",
            icon = icon("file-import"),
            width = "100%"
          )),
          div(
            id = ns("hinweis-laden"), class = "miniHinweis",
            "Hier gespeicherte Auswertungen können Sie unter 
                 'Individuelle Analysen' laden!"
          )
        )
      ),
      ##########################################################################
      # Erweiterte Filtereinstellungen
      filterDatenUI(id),
      ##########################################################################
      # Ergebnisse
      ##########################################################################
      fluidRow(
        box(
          title = "Ergebnisse",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          ### Hinweise versteckt (nur wenn kein Dia) ---------------------------
          hidden(
            div(
              id = ns("hinweis-zeroData"), class = "hinweis",
              "HINWEIS: Es wird kein Ergebnis erzeugt - keine gültigen Fälle enthalten!"
            )
          ),
          hidden(div(
            id = ns("hinweis-rowVar1"), class = "hinweis",
            "HINWEIS: Bitte wählen Sie eine Analysevariable aus!"
          )),
          hidden(div(
            id = ns("hinweis-colVar1"), class = "hinweis",
            "HINWEIS: Bitte wählen Sie eine Spaltenvariable aus!"
          )),
          # Download komplette Analyse -----------------------------------------
          downloadButton(ns("exportAnalyse"),
            label = "Export Excel (alle Tabellen und Grafik)"
          ),
          br(), br(),
          tabBox(
            height = "auto",
            width = 12,
            # ------------------------------------------------------------------
            tabPanel(
              "Grafik",
              fluidRow(
                ### Grafiktyp ändern -------------------------------------------
                column(
                  3,
                  selectizeInput(ns("plotType"),
                    label = "Grafikart ändern",
                    choices = choicesNominal,
                    selected = "Säulen (gestapelt)"
                  )
                )
              ),
              ### Grafikbereich ------------------------------------------------
              div(
                id = ns("plotoutput"),
                div(
                  id = "abbText",
                  verbatimTextOutput(ns("abbText"))
                ),
                withSpinner(plotOutput(ns("plot"),
                  height = 350
                )),
                downloadButton(ns("Export"),
                  label = "Export png",
                  class = "exportButton"
                ),
                downloadButton(ns("ExportEdit"),
                  label = "Export xlsx (editierbar)",
                  class = "exportButton"
                )
              ),
              ### erweitere Einstellungen Grafik -------------------------------
              div(
                id = ns("plotSettings"),
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
                      # Beschriftungen teilweise ausblenden?
                      checkboxInput(
                        ns("labelsHidePlot"),
                        label = HTML("Beschriftungen ausblenden<br/>(&le; ... %)"),
                        value = TRUE
                      )
                    ),
                    column(
                      3,
                      # welche Beschriftungen ausblenden?
                      sliderInput(
                        ns("hidePercentPlot"),
                        label = NULL,
                        min = 0, max = 100, post = "%",
                        value = 0
                      )
                    ),
                    column(
                      3,
                      # Beschriftungen ohne Prozentzeichen?
                      checkboxInput(
                        ns("percentHide"),
                        label = "Beschriftung ohne Prozentzeichen %",
                        value = FALSE
                      )
                    ),
                    column(
                      3,
                      # Textgröße Werte?
                      selectizeInput(ns("geomTextSize"),
                        label = HTML("Textgröße Werte:"),
                        choices = 14:24,
                        selected = 14
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      3,
                      # Textgröße Achsen/Legende?
                      selectizeInput(ns("baseSize"),
                        label = HTML("Textgröße Achsen/Legende:"),
                        choices = 14:24,
                        selected = 14
                      )
                    ),
                    column(
                      3,
                      # Position Legende?
                      selectizeInput(ns("legende"),
                        label = HTML("Postion Legende:"),
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
                      # Farbpalette?
                      selectizeInput(ns("color"),
                        label = HTML("Farbpalette:"),
                        choices = c(
                          "Orangetöne" = "Oranges",
                          "Blautöne" = "Blues",
                          "Grüntöne" = "Greens"
                        ),
                        selected = "Orangetöne"
                      )
                    ),
                    column(
                      3,
                      selectizeInput(ns("aspectRatio"),
                        label = "Seitenverhältnis Grafik:",
                        choices = c(
                          "21:9" = 21 / 9,          # 2.33
                          "2:1" = 2,                # 2
                          "16:9" = 16 / 9,          # 1.78
                          "Goldenes Rechteck" = phi # 1.62
                        ), 
                        selected = "21:9"
                      )
                    )
                  ),
                  # Kategorien ausblenden? -------------------------------------
                  selectizeInput(
                    ns("rowHide"),
                    label = "Kategorien der Analysevariable ausblenden (mind. eine Kategorie muss dargestellt werden):",
                    choices = c("Eine oder mehr Kategorien wählen" = ""),
                    selected = "",
                    multiple = TRUE
                  ),
                  selectizeInput(
                    ns("colHide"),
                    label = "Kategorien der Spaltenvariable ausblenden (mind. eine Kategorie muss dargestellt werden):",
                    choices = c("Eine oder mehr Kategorien wählen" = ""),
                    selected = "",
                    multiple = TRUE
                  ),
                  selectizeInput(
                    ns("rowHide2"),
                    label = "Kategorien der Unterteilungsvariable ausblenden (mind. eine Kategorie muss dargestellt werden):",
                    choices = c("Eine oder mehr Kategorien wählen" = ""),
                    selected = "",
                    multiple = TRUE
                  ),
                  # Liniendias / Punkte ----------------------------------------
                  # linearen Trend anzeigen ?
                  checkboxInput(ns("trend"),
                    label = "Linearen Trend anzeigen",
                    value = FALSE
                  ),
                  # Boxplot/Violinplot -----------------------------------------
                  # metrische Var: Boxplot anzeigen ?
                  checkboxInput(ns("boxplot"),
                    label = "Box-Plot anzeigen",
                    value = TRUE
                  ),
                  # metrische Var: Violin-Plot anzeigen ?
                  checkboxInput(ns("violin"),
                    label = "Violin-Plot anzeigen",
                    value = TRUE
                  ),
                  # metrische Var: Mittelwert anzeigen ?
                  checkboxInput(ns("mw"),
                    label = "Mittelwert anzeigen",
                    value = FALSE
                  ),
                  # Histogramm -------------------------------------------------
                  # Klassenanzahl auswählen
                  fluidRow(
                    column(
                      6,
                      sliderInput(ns("bins"),
                        label = "Anzahl Histrogramm-Klassen",
                        min = 5, max = 50, step = 1,
                        value = 20
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      # metrische Var: Mittelwert anzeigen ?
                      checkboxInput(ns("density"),
                        label = "Kern-Dichteschätzung anzeigen",
                        value = TRUE
                      )
                    )
                  )
                )
              ),
              # Hinweise versteckt (nur wenn kein Dia)
              hidden(div(
                id = ns("hinweis-noPlot"), class = "hinweis",
                "HINWEIS: Es wird kein Diagramm erzeugt da zu viele Ausprägungen der Analysevariable vorliegen. (Häufigkeitstabellen liegen vor.)"
              ))
            ), 
            # ------------------------------------------------------------------
            tabPanel(
              "Tabellen", "Tabellarische Auswertungen",
              # Relative und absolute Häufigkeitstabellen
              div(
                id = ns("tabOutput"),
                withSpinner(DT::dataTableOutput(ns("propTab"))),
                withSpinner(DT::dataTableOutput(ns("freTab"))),
                # Summary Lage/Streuung nur bei metrischen Var
                div(
                  id = ns("sumTable"),
                  withSpinner(DT::dataTableOutput(ns("sumTab")))
                ),
                withSpinner(DT::dataTableOutput(ns("zusammenhang")))
              )
            ),
            # ------------------------------------------------------------------
            tabPanel(
              "Weitere Infos", "Zusätzliche Informationen",
              withSpinner(DT::dataTableOutput(ns("infosColVar1"))),
              withSpinner(DT::dataTableOutput(ns("infosRowVar1"))),
              hidden(div(
                id = ns("divInfosRowVar2"),
                withSpinner(DT::dataTableOutput(ns("infosRowVar2")))
              )),
              withSpinner(DT::dataTableOutput(ns("filterInfo")))
            )
            ####################################################################
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
analysen <- function(input, output, session) {
  #
  ##############################################################################
  #### verschiedene Seiten:
  ##############################################################################
  # Zeitreihen -----------------------------------------------------------------
  if (session$ns("") == "zeit-") {
    hideElement(id = "colVarDiv")
    hideElement(id = "jahrFilter")
  # Jährliche Analysen ---------------------------------------------------------
  } else if (session$ns("") == "jahr-") {
    updateSelectizeInput(session, "jahrFilter",
      choices = rev(levels(daten[[sym(zeitVar)]])),
      selected = last(levels(daten[[sym(zeitVar)]]))
    )
  # individuelle Analysen ------------------------------------------------------
  } else if (session$ns("") == "analyse-") {
    showElement(id = "loadSettings")
    hideElement(id = "hinweis-laden")
  }
  ##############################################################################
  # Zeitreihen: Spaltenvariable Schuljahr --------------------------------------
  observe({
    if (session$ns("") == "zeit-" &&
      input$colVar1 != zeitVar) {
      updateSelectizeInput(session, "colVar1",
        choices = varsBereiche,
        selected = zeitVar
      )
      updateCheckboxInput(session, "missValuesColVar1", value = FALSE)
    }
  })
  # Jährliche Analysen: beim Jahr muss IMMER eine Angabe vorliegen -------------
  observe({
    if (session$ns("") == "jahr-" && input$jahrFilter == "") {
      updateSelectizeInput(session, "jahrFilter",
        choices = rev(levels(daten[[sym(zeitVar)]])),
        selected = last(levels(daten[[sym(zeitVar)]]))
      )
    }
  })
  # individuelle Analyse: jahrFilter: wenn leer dann Alle Jahre ----------------
  observe({
    if (session$ns("") == "analyse-" && input$jahrFilter == "") {
      reset("jahrFilter")
    }
  })
  ##############################################################################
  #### Filter : Code einbinden
  ##############################################################################
  # nicht über callModule, da der Namensraum erhalten bleiben muss
  source("scripts/module/filterDataServer.R", encoding = "utf8", local = TRUE)
  ##############################################################################
  #### Daten:
  ##############################################################################
  # (1) Daten filtern
  # myDataSet$data : Daten gefiltert (globale Filter und/oder erweiterte Filter)
  myDataSet <- reactiveValues(data = NULL)
  # Daten filtern : reaktiv oder neu berechnen wenn Button filterDo
  observe({
    req(input$rowVar1, input$colVar1)
    input$filterDo 
    myDataSet$data <- dataFilter(
      colVar1 = input$colVar1,
      missValuesColVar1 = input$missValuesColVar1,
      rowVar1 = input$rowVar1,
      missValuesRowVar1 = input$missValuesRowVar1,
      rowVar2 = input$rowVar2,
      missValuesRowVar2 = input$missValuesRowVar2,
      wohnMitte = input$wohnMitte,
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
  # resetFilter geklickt: erweiterte Filter zurücksetzen und neu berechnen
  observeEvent(input$resetFilter, {
    # Filter 1 löschen
    updateSelectizeInput(session, "filter1", selected = "")
    updateSelectizeInput(session, "operatorFilter1", selected = "gleich")
    updateSelectizeInput(session, "valueFilter1",
      choices = c("Eine oder mehr Kategorien wählen" = ""),
      selected = ""
    )
    # enable filter1
    enable("filter1")
    enable("operatorFilter1")
    enable("valueFilter1")
    # Filter 2 löschen
    updateSelectizeInput(session, "filter2", selected = "")
    updateSelectizeInput(session, "operatorFilter2", selected = "gleich")
    updateSelectizeInput(session, "valueFilter2",
      choices = c("Eine oder mehr Kategorien wählen" = ""),
      selected = ""
    )
    # enable filter2
    enable("filter2")
    enable("operatorFilter2")
    enable("valueFilter2")
    # Filter 3 löschen
    updateSelectizeInput(session, "filter3", selected = "")
    updateSelectizeInput(session, "operatorFilter3", selected = "gleich")
    updateSelectizeInput(session, "valueFilter3",
      choices = c("Eine oder mehr Kategorien wählen" = ""),
      selected = ""
    )
    # enable filter3
    enable("filter3")
    enable("operatorFilter3")
    enable("valueFilter3")
    # reaktive Werte löschen
    resetFilterVals()
    # Daten ohne erweitere Filter neu berechnen
    myDataSet$data <- dataFilter(
      colVar1 = input$colVar1,
      missValuesColVar1 = input$missValuesColVar1,
      rowVar1 = input$rowVar1,
      missValuesRowVar1 = input$missValuesRowVar1,
      rowVar2 = input$rowVar2,
      missValuesRowVar2 = input$missValuesRowVar2,
      wohnMitte = input$wohnMitte,
      jahrFilter = input$jahrFilter
    )
  })
  # ----------------------------------------------------------------------------
  # (2) mit gefilterten Daten die Analysetabelle erstellen : Häufigkeiten
  dat <- reactive({
    req(input$rowVar1, input$colVar1)
    req(myDataSet$data)
    req(rowsZero() == FALSE)
    dataBase(myDataSet$data, input$rowVar1, input$colVar1, input$rowVar2)
  })
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
    if (noCalculation() == FALSE) {
      rowsZero <- nrow(myDataSet$data) == 0 # TRUE
    } else {
      rowsZero <- FALSE
    }
  })
  # Outputs nicht anzeigen wenn es nichts zum anzeigen gibt --------------------
  observe({
    # keine Berechnungen oder keine Auswahl
    if (rowsZero() == TRUE || noCalculation() == TRUE ||
      input$rowVar1 == "" || input$colVar1 == "") {
      hideElement(id = "plotType")
      hideElement(id = "plotoutput")
      hideElement(id = "plotSettings")
      hideElement(id = "exportAnalyse")
      hideElement(id = "tabOutput")
      # andere Hinweise erst mal zurücksetzen
      hideElement(id = "hinweis-zeroData")
      hideElement(id = "hinweis-noPlot")
    }
    # keine Zeilenvariable ausgewählt
    if (input$rowVar1 == "") {
      showElement(id = "hinweis-rowVar1")
    } else {
      hideElement(id = "hinweis-rowVar1")
    }
    # keine Spaltenvariable ausgewählt
    if (input$colVar1 == "") {
      showElement(id = "hinweis-colVar1")
    } else {
      hideElement(id = "hinweis-colVar1")
    }
    # keine gültigen Fälle aber Auswahl der Variablen
    if (rowsZero() == TRUE && (input$rowVar1 != "" && input$colVar1 != "")) {
      showElement(id = "hinweis-zeroData")
    } else {
      hideElement(id = "hinweis-zeroData")
    }
    # alles an Berechnungen ok
    if (rowsZero() == FALSE && noCalculation() == FALSE &&
      (input$rowVar1 != "" && input$colVar1 != "")) {
      showElement(id = "plotType")
      showElement(id = "plotoutput")
      showElement(id = "plotSettings")
      showElement(id = "exportAnalyse")
      showElement(id = "tabOutput")
      # nur Häufigkeiten-Grafiken: falls zu viele Zeilen dann nicht darstellen
      # entweder Plot anzeigen ODER Hinweis
      if (metrisch() == FALSE &&
        input$plotType %in% choicesNominal &&
        nLev25() == FALSE) {
        # Grafik verbergen, Hinweis zeigen
        showElement(id = "hinweis-noPlot")
        hideElement(id = "plotType")
        hideElement(id = "plotoutput")
        hideElement(id = "plotSettings")
      } else {
        # Grafik anzeigen, Hiwweis verbergen
        hideElement(id = "hinweis-noPlot")
      }
    }
  })
  ##############################################################################
  #### Spaltenvariable
  ##############################################################################
  # die Zeilenvariable nicht als Auswahlmögichkeit bei Spalten zulassen
  observeEvent(input$rowVar1, {
    vars <- varsBereiche
    # bei jährl. Analyse braucht Schuljahr nicht angeboten werden
    if (session$ns("") == "jahr-") {
      vars <- vars[-2]
    }
    selected <- input$colVar1
    if (input$rowVar1 != "") {
      bereichRowVar1 <- meta$Bereich[which(meta$Variable == input$rowVar1)]
      # Auswahl colVar1 ohne rowVar1
      vars[[bereichRowVar1]] <- # Elemente im Listenelement Bereich rowVar1
        vars[[bereichRowVar1]][-which(vars[[bereichRowVar1]] == input$rowVar1)]
    }
    updateSelectizeInput(session, "colVar1",
      choices = c(
        list("Spaltenvariable auswählen" = ""),
        vars
      ),
      selected = selected
    )
  })
  ##############################################################################
  #### weitere Unterteilung der Auswertung ?
  ##############################################################################
  # bei der weiteren Unterteilung die Zeilen- + Spaltenvariable ausschließen!
  observeEvent(c(input$rowVar1, input$colVar1), {
    vars <- varsBereicheKat[-1] # Liste mit kategorialen Vars ohne Gesamt
    # bei jährl. Analyse braucht Schuljahr nicht angeboten werden
    if (session$ns("") == "jahr-") {
      vars <- vars[-1]
    }
    selected <- input$rowVar2 # das ausgewählte festhalten und übernehmen
    if (input$rowVar1 != "") {
      # ohne rowVar1
      bereichRowVar1 <- meta$Bereich[which(meta$Variable == input$rowVar1)]
      vars[[bereichRowVar1]] <- # Elemente im Listenelement Bereich rowVar1
        vars[[bereichRowVar1]][-which(vars[[bereichRowVar1]] == input$rowVar1)]
    }
    #
    if (input$colVar1 != "") {
      # ohne colVar1
      bereichColVar1 <- meta$Bereich[which(meta$Variable == input$colVar1)]
      vars[[bereichColVar1]] <- # Elemente im Listenelement Bereich colVar1
        vars[[bereichColVar1]][-which(vars[[bereichColVar1]] == input$colVar1)]
    }
    updateSelectizeInput(session, "rowVar2",
      choices = c(
        "Weitere Unterteilung auswählen" = "",
        vars
      ),
      selected = selected
    )
  })
  # ----------------------------------------------------------------------------
  # nur wenn weitere Unterteilung (rowVar2) ausgewählt:
  # Infotabelle anzeigen
  # erw. Einstellungen Grafik: "Kategorien für Unterteilung ausblenden" anzeigen
  observeEvent(input$rowVar2, {
    if (input$rowVar2 == "") {
      hideElement(id = "divInfosRowVar2")
      hideElement(id = "rowHide2")
    } else {
      showElement(id = "divInfosRowVar2")
      showElement(id = "rowHide2")
    }
    # für Punktediagramme: Boxplot/Dichte nur zulassen wenn keine Unterteilung
    if (input$plotType == "Punkte") {
      if (input$rowVar2 == "") {
        showElement("boxplot")
        showElement("density")
      } else {
        hideElement("boxplot")
        hideElement("density")
      }
    }
  })
  ##############################################################################
  #### metrische Variablen:
  ##############################################################################
  # metrisch? TRUE/FALSE
  # ZeilenVar metrisch?
  metrisch <- reactive({
    req(input$rowVar1)
    (meta[meta$Variable == input$rowVar1, "Meßart"] == "metrisch")
  })
  # Zeilen- und SpaltenVar metrisch?
  metrisch2 <- reactive({
    req(input$rowVar1, input$colVar1)
    (meta[meta$Variable == input$rowVar1, "Meßart"] == "metrisch") &&
      (meta[meta$Variable == input$colVar1, "Meßart"] == "metrisch")
  })
  # ----------------------------------------------------------------------------
  # zusätzliche Tabelle und Grafiktyp(en) anzeigen; Einstellungen anpassen
  # zuerst aufpassen, dass bei plotType IMMER was gewählt ist
  observeEvent(c(metrisch(), metrisch2(), input$plotType), {
    if (input$plotType == "") {
      updateSelectizeInput(session, "plotType",
        choices = c(
          choicesNominal,
          choicesMetrisch,
          choicesMetrisch2
        ),
        selected = "Säulen (gestapelt)"
      )
      # metrische Var:
      if (metrisch()) {
        if (nLev25() == FALSE) { # zu viele Ausprägungen : keine Häufigkeiten
          if (metrisch2()) { # beide Variablen metrisch
            updateSelectizeInput(session, "plotType",
              choices = c(
                choicesMetrisch,
                choicesMetrisch2
              ),
              selected = "Punkte"
            )
          } else {
            updateSelectizeInput(session, "plotType",
              choices = c(choicesMetrisch),
              selected = "Boxplot/Violinplot"
            )
          }
        } else { # weniger als 25 Ausprägungen: alle plotType zugelassen
          if (metrisch2()) { # beide Variablen metrisch
            updateSelectizeInput(session, "plotType",
              choices = c(
                choicesNominal,
                choicesMetrisch,
                choicesMetrisch2
              ),
              selected = "Säulen (gestapelt)"
            )
          } else {
            updateSelectizeInput(session, "plotType",
              choices = c(
                choicesNominal,
                choicesMetrisch
              ),
              selected = "Säulen (gestapelt)"
            )
          }
        }
        # nominale Var:
      } else {
        updateSelectizeInput(session, "plotType",
          choices = choicesNominal,
          selected = "Säulen (gestapelt)"
        )
      }
    }
  }, priority = 20)
  # Seitenverhältnis muss immer gewählt sein -----------------------------------
  observeEvent(input$aspectRatio, {
    if (input$aspectRatio == "") {
      reset("aspectRatio")
    }
  }, priority = 19)
  # Legende muss immer gewählt sein --------------------------------------------
  observeEvent(input$legende, {
    if (input$legende == "") {
      reset("legende")
    }
  }, priority = 19)
  # Farbe muss immer gewählt sein ----------------------------------------------
  observeEvent(input$color, {
    if (input$color == "") {
      reset("color")
    }
  }, priority = 19)
  # Textgrößen müssen immer gewählt sein ---------------------------------------
  observeEvent(input$geomTextSize, {
    if (input$geomTextSize == "") {
      reset("geomTextSize")
    }
  }, priority = 19)
  observeEvent(input$baseSize, {
    if (input$baseSize == "") {
      reset("baseSize")
    }
  }, priority = 19)
  # Grafikauswahl abhängig vom skalenniveau ------------------------------------
  observeEvent(c(metrisch(), nLev25(), metrisch2()), {
    selected <- input$plotType
    # metrische Variablen:
    if (metrisch()) {
      showElement(id = "sumTable")
      if (nLev25() == FALSE) { # zu viele Ausprägungen
        if (metrisch2()) {
          selected <- "Punkte"
          updateSelectizeInput(session, "plotType",
            choices = c(
              choicesMetrisch,
              choicesMetrisch2
            ),
            selected = selected
          )
        } else {
          if (!(selected %in% choicesMetrisch)) {
            selected <- "Boxplot/Violinplot"
          }
          updateSelectizeInput(session, "plotType",
            choices = c(choicesMetrisch),
            selected = selected
          )
        }
      } else { # metrisch, weniger als 25 Ausprägungen: alle plotType
        if (metrisch2()) {
          selected <- "Punkte"
          updateSelectizeInput(session, "plotType",
            choices = c(
              choicesNominal,
              choicesMetrisch,
              choicesMetrisch2
            ),
            selected = selected
          )
        } else {
          updateSelectizeInput(session, "plotType",
            choices = c(
              choicesNominal,
              choicesMetrisch
            ),
            selected = selected
          )
        }
      }
      # nominale Variablen:
    } else {
      hideElement(id = "sumTable")
      if (!(selected %in% choicesNominal)) {
        selected <- "Säulen (gestapelt)"
      }
      updateSelectizeInput(session, "plotType",
        choices = choicesNominal,
        selected = selected
      )
    }
  })
  # ----------------------------------------------------------------------------
  # Daten metrisch: Datenspalte für ZeilenVar als numeric speichern
  datNum <- reactive({
    req(myDataSet$data)
    if (metrisch() == TRUE) {
      datNumVar(myDataSet$data, input$rowVar1)
    }
  })
  ##############################################################################
  # (3) Ausgabetabellen:
  ##############################################################################
  # Prozenttabelle - unformatiert ----------------------------------------------
  tabPr <- reactive({
    if (input$rowVar2 == "") {
      tab <- tabPro(dat())
    } else {
      tab <- tabPro2(dat(), input$rowVar1, input$rowVar2, input$colVar1)
    }
    return(tab)
  })
  # Prozenttabelle - als % formatiert ------------------------------------------
  tabPrFormat <- reactive({
    tab <- tabPr()
    if (input$rowVar2 == "") {
      tab[, -1] <- lapply(tab[, -1] * 100, sprintf, fmt = "%1.1f%%")
    } else {
      tab[, -c(1, 2)] <- lapply(tab[, -c(1, 2)] * 100, sprintf, fmt = "%1.1f%%")
    }
    return(tab)
  })
  # Häufigkeitstabelle ---------------------------------------------------------
  tabFre <- reactive({
    if (input$rowVar2 == "") {
      tab <- tabFreq(dat())
    } else {
      tab <- tabFreq2(dat(), input$rowVar1, input$rowVar2, input$colVar1)
    }
    return(tab)
  })
  # Tabelle Lage/Streuung bei metrischer Var -----------------------------------
  tabSummary <- reactive({
    req(datNum())
    tabSum(datNum(), input$colVar1, input$rowVar2, metrisch2())
  })
  ##############################################################################
  #### Grafiken:
  ##############################################################################
  # zuerst die Datentabelle Häufigkeitsdiagramme erstellen
  plotData <- reactive({
    req(dat())
    data <- dat()
    # wenn gesamt-Variable: für die Grafik das Level rausnehmen
    if (any(colnames(data) == "Gesamt")) {
      levels(data$Gesamt) <- ""
    }
    # tidy evaluation
    isolate({
      rowVar1 <- sym(input$rowVar1)
      colVar1 <- sym(input$colVar1)
      rowVar2 <- sym(input$rowVar2)
      groupVars <- syms(c(input$colVar1, input$rowVar2))
    })
    # 0-Zeilen auffüllen, nur wenn Levels vorhanden
    data <- data %>%
      ungroup() %>%
      droplevels() %>%
      group_by(!!!groupVars) %>%
      complete((!!rowVar1),
        fill = list(n = 0, prop = 0, ratio = "0%")
      )
    # Erweiterte Einstellungen Grafik - soll reaktiv sein ----------------------
    # Beschriftungen ausblenden?
    if (input$labelsHidePlot == TRUE) { # nur wenn gerundet < hidePercent
      data <- data %>%
        mutate(ratio = ifelse(round(prop * 100) <= input$hidePercentPlot,
          "", ratio
        ))
    }
    # Beschriftung ohne Prozentzeichen?
    if (input$percentHide == TRUE) {
      data <- data %>% mutate(ratio = str_sub(ratio, end = -2L))
    }
    # Kategorien ausblenden?
    if (length(input$rowHide) > 0) {
      data <- data %>% filter(!(!!rowVar1) %in% input$rowHide)
    }
    if (length(input$colHide) > 0) {
      data <- data %>% filter(!(!!colVar1) %in% input$colHide)
    }
    if (input$rowVar2 != "" && length(input$rowHide2) > 0) {
      data <- data %>% filter(!(!!rowVar2) %in% input$rowHide2)
    }
    return(data)
  })
  # ----------------------------------------------------------------------------
  # Grafik - für output + Export
  plot <- reactive({
    req(input$rowVar1, input$colVar1, input$plotType, input$aspectRatio, dat())
    if (metrisch2() == TRUE) {req(input$plotType == "Punkte")}
    if ((input$plotType %in% c(choicesMetrisch, choicesMetrisch2))
    && metrisch() == TRUE) {
      # metrische Diagrammarten
      plot <- plotNum(datNum(),
        rowVar1 = input$rowVar1,
        colVar1 = input$colVar1,
        rowVar2 = input$rowVar2,
        rowHide2 = input$rowHide2,
        colHide = input$colHide,
        plotType = input$plotType,
        boxplot = input$boxplot,
        violin = input$violin,
        mw = input$mw,
        bins = input$bins,
        density = input$density,
        baseSize = input$baseSize,
        trend = input$trend
      )
    } else {
      # kategoriale Diagrammarten
      plot <- plotCat(
        data = plotData(),
        col = colBars(dat(), input$color),
        rowVar1 = input$rowVar1,
        colVar1 = input$colVar1,
        rowVar2 = input$rowVar2,
        plotType = input$plotType,
        legende = input$legende,
        trend = input$trend,
        baseSize = input$baseSize,
        geomTextSize = input$geomTextSize
      )
    }
    return(plot)
  })
  # ----------------------------------------------------------------------------
  # Text für die Beschriftungen 
  txtInhalt <- reactive({
    req(input$rowVar1, input$colVar1)
    text <- paste(varLabels[input$rowVar1], "-", varLabels[input$colVar1])
    if (input$rowVar2 != "") {
      text <-
        paste0(text, " (Unterteilung nach: ", varLabels[input$rowVar2], ")")
    }
    return(text)
  })
  # ----------------------------------------------------------------------------
  # ab 25 Levels keine Häufigkeiten-Grafik!
  # TRUE: weniger als 25 Level
  nLev25 <- reactive({
    req(dat())
    len <- dat()[[ncol(dat()) - 3]] %>%
      droplevels() %>%
      levels() %>%
      length()
    return(len < 25 & !is.null(len))
  })
  # ----------------------------------------------------------------------------
  # erweiterte Einstellungen je nach Grafiktyp
  observeEvent(input$plotType, {
    if (input$plotType %in% c(choicesMetrisch, choicesMetrisch2)) {
      # metrische Var
      if (input$plotType == "Boxplot/Violinplot") { # nur Boxplot
        showElement(id = "aspectRatio")
        hideElement(id = "trend")
        showElement(id = "boxplot")
        showElement(id = "violin")
        showElement(id = "mw")
        hideElement(id = "bins")
        hideElement(id = "density")
        showElement(id = "colHide")
      }
      if (input$plotType == "Histogramm/Dichte") { # nur Histogramm
        showElement(id = "aspectRatio")
        hideElement(id = "trend")
        hideElement(id = "boxplot")
        hideElement(id = "violin")
        hideElement(id = "mw")
        showElement(id = "bins")
        showElement(id = "density")
        showElement(id = "colHide")
      }
      if (input$plotType == "Punkte") { # nur Punkte
        hideElement(id = "aspectRatio")
        showElement(id = "trend")
        showElement(id = "boxplot")
        hideElement(id = "violin")
        hideElement(id = "mw")
        hideElement(id = "bins")
        showElement(id = "density")
        hideElement(id = "colHide")
        updateCheckboxInput(session, "boxplot", value = TRUE)
        updateCheckboxInput(session, "density", value = FALSE)
      }
      hideElement(id = "color")
      hideElement(id = "labelsHidePlot")
      hideElement(id = "hidePercentPlot")
      hideElement(id = "geomTextSize")
      hideElement(id = "rowHide")
      hideElement(id = "legende")
      hideElement(id = "percentHide")
    } else {
      # nominale Var
      if (input$plotType == "Linien") { # nur Linien
        showElement(id = "trend")
        updateCheckboxInput(session, "labelsHidePlot", value = FALSE)
      }
      else {
        hideElement(id = "trend")
        updateCheckboxInput(session, "labelsHidePlot", value = TRUE)
      }
      showElement(id = "aspectRatio")
      hideElement(id = "boxplot")
      hideElement(id = "violin")
      hideElement(id = "mw")
      hideElement(id = "bins")
      hideElement(id = "density")
      showElement(id = "color")
      showElement(id = "labelsHidePlot")
      showElement(id = "hidePercentPlot")
      showElement(id = "geomTextSize")
      showElement(id = "rowHide")
      showElement(id = "colHide")
      showElement(id = "legende")
      showElement(id = "percentHide")
    }
  })
  # ----------------------------------------------------------------------------
  # nur für Punkte: es kann nur entweder Boxplot oder Dichte dargestellt werden
  observeEvent(input$boxplot, {
    if (input$plotType == "Punkte") {
      if (input$boxplot == TRUE) {
        updateCheckboxInput(session, "density", value = FALSE)
      }
    }
  }, priority = 20)
  observeEvent(input$density, {
    if (input$plotType == "Punkte") {
      if (input$density == TRUE) {
        updateCheckboxInput(session, "boxplot", value = FALSE)
      }
    }
  }, priority = 20)
  # ----------------------------------------------------------------------------
  # nur für Boxplot/Violinplot: eines von beiden muss dargestellt werden
  observeEvent(input$boxplot, {
    if (input$plotType == "Boxplot/Violinplot") {
      if (input$boxplot == FALSE) {
        updateCheckboxInput(session, "violin", value = TRUE)
      }
    }
  }, priority = 20)
  observeEvent(input$violin, {
    if (input$plotType == "Boxplot/Violinplot") {
      if (input$violin == FALSE) {
        updateCheckboxInput(session, "boxplot", value = TRUE)
      }
    }
  }, priority = 20)
  # ----------------------------------------------------------------------------
  # für "Kategorien ausblenden": Kategorien je nach Variable;
  # nur enthaltene Kategorien anbieten
  # in reactiveValues zwischenspeichern
  # ----------------------------------------------------------------------------
  hideValues <- reactiveValues(
    col = NULL,
    row1 = NULL,
    row2 = NULL
  )
  # rowVar1 --------------------------------------------------------------------
  observeEvent(input$rowVar1, {
    hideValues$row1 <- ""
  }, priority = 7)
  observeEvent(input$rowHide, {
    # wenn Ausprägungen verborgen werden sollen: hier zwischenspeichern
    hideValues$row1 <- input$rowHide
  }, priority = 6)
  # rowVar1
  observeEvent(dat(), {
    req(dat())
    # alle vorhandenen Kategorien als Vektor speichern
    choices <- dat() %>%
      ungroup() %>%
      distinct(!!sym(input$rowVar1)) %>%
      pull() %>%
      as.character()
    # Levels in der richtigen Reihenfolge, aber nur die die vorhanden sind
    choicesToSelect <-
      levels(daten[[input$rowVar1]])[levels(daten[[input$rowVar1]]) %in% choices]
    if (all(hideSet$valuesRowHide == "")) {
      # es wurde vorher nichts geladen
      selected <- hideValues$row1
      if (!all(selected %in% choicesToSelect)) {
        selected <- ""
      }
      updateSelectizeInput(session, "rowHide",
        choices = choicesToSelect,
        selected = selected,
        server = TRUE,
        options = list(
          maxItems =
            length(choicesToSelect) - 1 # mind. eine Kategorie für Darstellung
        )
      )
    } else {
      # es wurde vorher etwas geladen
      updateSelectizeInput(session, "rowHide",
        choices = choicesToSelect,
        selected = hideSet$valuesRowHide,
        server = TRUE,
        options = list(
          maxItems =
            length(choicesToSelect) - 1 # mind. eine Kategorie für Darstellung
        )
      )
      # reactiveValue zurücksetzen
      hideSet$valuesRowHide <- ""
    }
  })
  # ----------------------------------------------------------------------------
  # colVar1
  observeEvent(input$colVar1, {
    hideValues$col <- ""
  }, priority = 7)
  observeEvent(input$colHide, {
    # wenn Spalten verborgen werden sollen: hier zwischenspeichern
    hideValues$col <- input$colHide
  }, priority = 6)
  # bei colVar1 muss unterschieden werden ob metrische oder andere Grafiktypen
  observeEvent(c(dat(), input$plotType), {
    req(dat(), input$plotType)
    # alle vorhandenen Kategorien als Vektor speichern
    if (!input$plotType %in% choicesMetrisch) {
      choices <- dat() %>%
        ungroup() %>%
        distinct(!!sym(input$colVar1)) %>%
        pull() %>%
        as.character()
    } else { # metrisch: andere Datentabelle
      choices <- dat() %>%
        ungroup() %>%
        filter(!!sym(input$rowVar1) != "(fehlende Werte)") %>%
        distinct(!!sym(input$colVar1)) %>%
        pull() %>%
        as.character()
    }
    # Levels in der richtigen Reihenfolge, aber nur die die vorhanden sind
    choicesToSelect <-
      levels(daten[[input$colVar1]])[levels(daten[[input$colVar1]]) %in% choices]
    #
    if (all(hideSet$valuesColHide == "")) {
      # es wurde vorher nichts geladen
      selected <- hideValues$col
      if ((!all(selected %in% choicesToSelect) ||
        input$plotType == "Punkte")) {
        selected <- ""
        hideValues$col <- ""
      }
      #
      updateSelectizeInput(session, "colHide",
        choices = choicesToSelect,
        selected = selected,
        server = TRUE,
        options = list(
          maxItems =
            length(choicesToSelect) - 1 # mind. eine Kategorie für Darstellung
        )
      )
    } else {
      # es wurde vorher etwas geladen
      updateSelectizeInput(session, "colHide",
        choices = choicesToSelect,
        selected = hideSet$valuesColHide,
        server = TRUE,
        options = list(
          maxItems =
            length(choicesToSelect) - 1 # mind. eine Kategorie für Darstellung
        )
      )
      # reactiveValue zurücksetzen
      hideSet$valuesColHide <- ""
    }
  }, priority = 5)
  # ----------------------------------------------------------------------------
  # rowVar2
  observeEvent(input$rowVar2, {
    hideValues$row2 <- ""
  }, priority = 7)
  observeEvent(input$rowHide2, {
    # wenn Spalten verborgen werden sollen: hier zwischenspeichern
    hideValues$row2 <- input$rowHide2
  }, priority = 6)
  observeEvent(dat(), {
    req(dat(), input$rowVar2)
    # alle vorhandenen Kategorien als Vektor speichern
    choices <- dat() %>%
      ungroup() %>%
      distinct(!!sym(input$rowVar2)) %>%
      pull() %>%
      as.character()
    # Levels in der richtigen Reihenfolge, aber nur die die vorhanden sind
    choicesToSelect <-
      levels(daten[[input$rowVar2]])[levels(daten[[input$rowVar2]]) %in% choices]
    #
    if (all(hideSet$valuesRowHide2 == "")) {
      # es wurde vorher nichts geladen
      selected <- hideValues$row2
      if (!all(selected %in% choicesToSelect)) {
        selected <- ""
      }
      updateSelectizeInput(session, "rowHide2",
        choices = choicesToSelect,
        selected = selected,
        server = TRUE,
        options = list(
          maxItems =
            length(choicesToSelect) - 1 # mind. eine Kategorie für Darstellung
        )
      )
    } else {
      # es wurde vorher etwas geladen
      updateSelectizeInput(session, "rowHide2",
        choices = choicesToSelect,
        selected = hideSet$valuesRowHide2,
        server = TRUE,
        options = list(
          maxItems =
            length(choicesToSelect) - 1 # mind. eine Kategorie für Darstellung
        )
      )
      # reactiveValue zurücksetzen
      hideSet$valuesRowHide2 <- ""
    }
  })

  ##############################################################################
  # Info / Metadaten-Tabelle für Variablen erzeugen
  ##############################################################################
  infosRowVar1 <- reactive({
    req(input$rowVar1)
    createInfoTab(input$rowVar1)
  })
  infosColVar1 <- reactive({
    req(input$colVar1)
    createInfoTab(input$colVar1)
  })
  infosRowVar2 <- reactive({
    req(input$rowVar2)
    createInfoTab(input$rowVar2)
  })
  # Zusammenhangsanalysen ------------------------------------------------------
  zushg <- reactive({
    req(myDataSet$data, input$rowVar1, input$colVar1)
    # Fehlerbehandlung: kann ein Test berechnet werden?
    if (input$rowVar2 == "") {
      if (nrow(tabFre()) == 2 || ncol(tabFre()) == 2) {
        coeff <-
          data.frame(
            "Maßzahl" = NA,
            "Wert" = NA,
            "Interpretation" =
              "Keine Maßzahlen wegen unzureichender Dimensionalität der Tabelle."
          )
      } else {
        coeff <-
          coeff(myDataSet$data, input$rowVar1, input$colVar1, input$rowVar2)
      }
    } else {
      # wenn bei Unterteilung gar keine Maßzahlen möglich:
      # (Fehlerbehandlung bei einzelnen Untertabellen in Funktion)
      nRowTab <- myDataSet$data[[input$rowVar2]] %>%
        droplevels() %>%
        levels() %>%
        length()
      if (nrow(tabFre()) == nRowTab * 2 || ncol(tabFre()) == 3) {
        coeff <-
          data.frame(
            "Maßzahl" = NA,
            "Wert" = NA,
            "Interpretation" =
              "Keine Maßzahlen wegen unzureichender Dimensionalität der Tabellen."
          )
      } else {
        coeff <-
          coeff(myDataSet$data, input$rowVar1, input$colVar1, input$rowVar2)
      }
    }
    return(coeff)
  })
  ##############################################################################
  ##############################################################################
  #### OUTPUTS
  ##############################################################################
  ##############################################################################
  # Abbildung Caption ----------------------------------------------------------
  output$abbText <- renderText({
    paste("Abbildung: ", txtInhalt())
  })
  ### Tab Grafik ###############################################################
  # Diagramm -------------------------------------------------------------------
  observe({ 
    output$plot <- renderPlot({
      # Häufigkeiten Diagramme
      if (input$plotType %in% choicesNominal) {
        # nur wenn möglich (bis 25 Levels)
        if (nLev25() == TRUE) {
          print(plot())
        }
      }
      # metrische Diagramme
      if (input$plotType %in% choicesMetrisch) {
        print(plot())
      }
      # nur Punktediagramme
      if (input$plotType %in% choicesMetrisch2) {
        if (!(input$boxplot == TRUE && input$density == TRUE)) {
          print(plot())
        }
      }
    }, height = 350, width = 350 * as.numeric(input$aspectRatio)) 
  }, priority = 1)
  # Download Plot --------------------------------------------------------------
  # png
  output$Export <- downloadHandler(
    filename = function() {
      if (input$rowVar2 == "") {
        paste0(
          "Grafik_", input$rowVar1, "_",
          input$colVar1, ".png"
        )
      } else {
        paste0(
          "Grafik_", input$rowVar1, "_", input$rowVar2, "_",
          input$colVar1, ".png"
        )
      }
    },
    content = function(file) {
      ggsave(file,
        plot = plot(),
        width = 5 * as.numeric(input$aspectRatio), height = 5
      )
    }
  )
  # Excel (editierbare Vektor-Grafik)
  output$ExportEdit <- downloadHandler(
    filename = function() {
      if (input$rowVar2 == "") {
        paste0(
          "Grafik_", input$rowVar1, "_",
          input$colVar1, ".xlsx"
        )
      } else {
        paste0(
          "Grafik_", input$rowVar1, "_", input$rowVar2, "_",
          input$colVar1, ".xlsx"
        )
      }
    },
    content = function(file) {
      openxlsxwb <- createWorkbook()
      addWorksheet(openxlsxwb, "Grafik", gridLines = F)
      writeData(openxlsxwb, sheet = "Grafik", txtInhalt())
      ggsave("plot.png",
        plot = plot(),
        width = 5 * as.numeric(input$aspectRatio), height = 5
      )
      insertImage(openxlsxwb,
        "Grafik", "plot.png",
        width = 5 * as.numeric(input$aspectRatio),
        height = 5, startRow = 3
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
        code = print(plot()),
        width = 5 * as.numeric(input$aspectRatio),
        height = 5, left = 0, top = 0
      )
      print(officerwb, target = file)
      if (file.exists("plot.png")) {
        file.remove("plot.png")
      }
    }
  )
  # Tab Tabellen ###############################################################
  # Prozenttabelle -------------------------------------------------------------
  output$propTab <- renderDT(
    DT::datatable(tabPrFormat(),
      caption = paste(
        "Tabelle Relative Häufigkeiten:",
        txtInhalt()
      ),
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
  # Häufigkeiten ---------------------------------------------------------------
  output$freTab <- renderDT(
    DT::datatable(tabFre(),
      caption = paste(
        "Tabelle Absolute Häufigkeiten:",
        txtInhalt()
      ),
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
  # Summary Lage/Streuung ------------------------------------------------------
  output$sumTab <- renderDT(
    DT::datatable(tabSummary(),
      caption = paste(
        "Tabelle Lage- und Streuungsmaße:",
        txtInhalt()
      ),
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
  # Zusammenhangsmaße ----------------------------------------------------------
  output$zusammenhang <- renderDT(
    DT::datatable(suppressWarnings(zushg()),
      caption = paste("Zusammenhangsmaße: ", txtInhalt()),
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
  output$infosColVar1 <- renderDT(
    DT::datatable(infosColVar1(),
      caption = paste(
        "Meta-Informationen: ",
        varLabels[input$colVar1]
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
  output$infosRowVar2 <- renderDT(
    DT::datatable(infosRowVar2(),
      caption = paste(
        "Meta-Informationen: ",
        varLabels[input$rowVar2]
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
  # Export komplette Analyse
  output$exportAnalyse <- downloadHandler(
    filename = function() {
      if (input$rowVar2 == "") {
        paste0(
          "Analyse_",
          input$rowVar1, "_",
          input$colVar1, ".xlsx"
        )
      } else {
        paste0(
          "Analyse_",
          input$rowVar1, "_",
          input$rowVar2, "_",
          input$colVar1, ".xlsx"
        )
      }
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
      # sheet Lage_Streuung ----------------------------------------------------
      if (metrisch() == TRUE) {
        addWorksheet(wb, "Lage_Streuung")
        writeData(wb,
          sheet = "Lage_Streuung",
          paste("Tabelle Lage- und Streuungsmaße:", txtInhalt())
        )
        writeData(wb,
          sheet = "Lage_Streuung",
          tabSummary(), startRow = 3
        )
      }
      # sheet Zusammenhangsmaße ------------------------------------------------
      addWorksheet(wb, "Zusammenhangsmaße")
      writeData(wb,
        sheet = "Zusammenhangsmaße",
        paste("Tabelle Zusammenhangsmaße:", txtInhalt())
      )
      writeData(wb, sheet = "Zusammenhangsmaße", zushg(), startRow = 3)
      # sheet Metadaten --------------------------------------------------------
      addWorksheet(wb, "Metadaten")
      writeData(wb, sheet = "Metadaten", paste("Metadaten:", txtInhalt()))
      writeData(wb, sheet = "Metadaten", infosColVar1(), startRow = 3)
      writeData(wb, sheet = "Metadaten", infosRowVar1(), startRow = 10)
      if (input$rowVar2 == "") {
        writeData(wb, sheet = "Metadaten", "Filterinformationen:", startRow = 17)
        writeData(wb, sheet = "Metadaten", filterTab(), startRow = 18)
      } else {
        writeData(wb, sheet = "Metadaten", infosRowVar2(), startRow = 17)
        writeData(wb, sheet = "Metadaten", "Filterinformationen:", startRow = 24)
        writeData(wb, sheet = "Metadaten", filterTab(), startRow = 25)
      }
      # sheet Grafik -----------------------------------------------------------
      # (nur wenn Grafik vorhanden sein kann!)
      if (nLev25() == TRUE ||
        input$plotType %in% c(choicesMetrisch, choicesMetrisch2)) {
        addWorksheet(wb, "Grafik", gridLines = F)
        writeData(wb, sheet = "Grafik", txtInhalt())
        ggsave("plot.png",
          plot = plot(),
          width = 5 * as.numeric(input$aspectRatio), height = 5
        )
        insertImage(wb, "Grafik", "plot.png",
          width = 5 * as.numeric(input$aspectRatio),
          height = 5, startRow = 3
        )
        # sheet editierbare Grafik (temporäres xlsx-file)
        tmpwb <- tempfile(fileext = ".xlsx")
        suppressMessages(saveWorkbook(wb, tmpwb))
        officerwb <- read_xlsx(tmpwb)
        file.remove(tmpwb)
        # Export plot as editable graphic in xlsx
        officerwb <- add_sheet(officerwb, label = "Grafik (editierbar)")
        officerwb <- xl_add_vg(officerwb,
          sheet = "Grafik (editierbar)",
          code = print(plot()),
          width = 5 * as.numeric(input$aspectRatio),
          height = 5, left = 0, top = 0
        )
        # ----------------------------------------------------------------------
        print(officerwb, target = file)
      } else {
        suppressMessages(saveWorkbook(wb, file))
        # ----------------------------------------------------------------------
      }
      if (file.exists("plot.png")) {
        file.remove("plot.png")
      }
    }
  )
  ##############################################################################
  # Einstellungen zurücksetzen/speichern/laden
  ##############################################################################
  source("scripts/module/saveLoadResetServer.R",
    encoding = "utf8", local = TRUE
  )
  ##############################################################################
}
#
################################################################################
