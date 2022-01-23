################################################################################
# ESU explorer
#
# help.R (module, Seite "Hilfe")
# Ulrike Niemann
#
################################################################################
# Module UI function:
helpUI <- function(id) {
  ns <- shiny::NS(id)
  #
  tagList(
    fluidRow(
      # ------------------------------------------------------------------------
      fluidRow(
        column(
          12,
          HTML("
               <ul>
               <li><h4>Inhalt:</h4></li>
               <ul>
               <li><a href='#seiten'>Verschiedene Analyseseiten</a></li>
               <li><a href='#eingaben'>Notwendige Eingaben und Einstellungen</a></li>
               <li><a href='#ergebnisse'>Ergebnisse</a></li>
               <li><a href='#help-erweitert'>Erweiterte Einstellungen (Grafik- + Filtereinstellungen)</a></li>
               <li><a href='#saveLoad'>Zurücksetzen, Speichern und Laden von Einstellungen</a></li>
               </a></li>
               </ul>
               </ul>
               "),
          box(
            title = HTML("<span id='seiten'>Verschiedene Analyseseiten</span>"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            "Die verschiedenen Analyseseiten können Sie über die Einstiegsseite 
                 oder über das Seitenmenü auswählen.",
            br(),
            HTML("Unter 
                 <i class='fa fa-chart-line'></i> <b>Zeitreihen Analysen</b> 
                 können Sie die Ergebnisse 
                 im Zeitverlauf über die Schuljahre 2010 - 2019 analysieren.
                 Als Spaltenvariable ist hierbei das Schuljahr fest zugewiesen.
                 "),
            br(),
            HTML("Unter 
                 <i class='fa fa-chart-bar'></i> <b>Jährliche Analysen</b> 
                 können Sie die Ergebnisse 
                 eines einzelnen Schuljahres analysieren.
                 Das entsprechende Schuljahr ist oben rechts auszuwählen. 
                 Standardmäßig ist das letzte verfügbare Schuljahr ausgewählt.
                 "),
            br(),
            HTML("Unter 
                 <i class='fa fa-table'></i> <b>Individuelle Analysen</b>
                 können Sie Ihre Analyse frei bestimmen. Es liegen keine Einschränkungen vor.
                 Die drei Analyse-Seiten unterscheiden sich somit nur in den Voreinstellungen,
                 die unter Zeitreihen und Jährliche Analysen getroffen werden.
                 "),
            br(),
            HTML("Unter 
                 <i class='fa fa-map'></i> <b>Karten</b> 
                 können Sie kleinräumige Analysen auf Basis der Lebensweltlich orientierten Räume 
                 (LOR) und der Einschulungsbereiche (ESB) durchführen. Die Ergebnisse 
                 werden auf einer Karte von Berlin Mitte dargestellt.
                ")
          )
        )
      ),
      # ------------------------------------------------------------------------
      fluidRow(
        column(
          12,
          box(
            title = HTML(
              "<span id='eingaben'>Notwendige Eingaben und Einstellungen</span>
              "),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            "In den ",
            HTML("<span style='background-color:#3c8dbc; color:white;'>blauen</span>"),
                 "Boxen im oberen Bereich der Analyseseiten werden Eingaben
                 von Ihnen erwartet. Wenn Eingaben für eine Analyse notwendig sind, 
                 erhalten Sie einen entsprechenden Hinweis in roter Schrift in der Ergebnis-Box.",
            br(),
            "Hier können Sie beispielsweise die Analysevariablen auswählen 
            und häufige Filter wählen.",
            br(),
            fluidRow(
              column(
                4,
                box(
                  title = "Analysevariable auswählen",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  selectizeInput(ns("rowVar1"),
                    label = "Analysevariable",
                    choices = c(
                      list("Analysevariable auswählen" = ""),
                      varsBereiche
                    ),
                    selected = ""
                  )
                )
              ),
              column(
                8,
                "Die Variablen sind thematisch in verschiedene Bereiche untergliedert, bspw. 
                Bereich ",
                HTML(
                "<i>Bildung</i> oder Bereich <i>Zuwanderungserfahrung</i>.<br>
                Sie können eine <u>Variable über das Dropdown-Menü auswählen</u>
                (Symbol <i class='fas fa-caret-down'></i> im Eingabefeld)
                <br><b>ODER</b><br>
                Sie können eine <u>dynamische Suche durchführen indem Sie direkt 
                in das Eingabefeld einen Text eingeben</u>.<br>
                Dazu klicken Sie bitte in das Eingabefeld. 
                Wenn Sie im Feld beispielsweise beginnen den Text <i>Zuwanderung</i>
                zu tippen sehen Sie dynamisch, welche Variablen diesen Text enthalten.")
              )
            ),
            fluidRow(
              column(
                12,
                "Sie können eine einzelne getroffene Auswahl löschen indem Sie mit der Maus auf das 
                Eingabefeld klicken und 
                anschließend die Zurück-/Backspace-Taste",
                HTML(
                "<i class='fas fa-long-arrow-alt-left'></i> auf Ihrer Tastatur drücken.<br>
                Das Programm reagiert auf diese Eingaben automatisch. Das heißt, wenn Sie hier 
                Änderungen vornehmen, wird die entsprechende Analyse automatisch aktualisiert.")
              )
            )
          )
        )
      ),
      # ------------------------------------------------------------------------
      fluidRow(
        column(
          12,
          box(
            title = HTML("<span id='ergebnisse'>Ergebnisse</span>"),
            width = 12,
            solidHeader = TRUE,
            status = "success",
            "In den ",
            HTML("<span style='background-color:#00a65a; color:white;'>grünen</span>"
                 ),
            " Boxen werden die Ergebnisse Ihrer Analyse dargestellt.
            Hier finden Sie Grafiken, Tabellen (bspw. Kreuztabellen,
            Angaben von statistischen Maßzahlen) sowie weitere Informationen
            zu den verwendeten Variablen und aktiven Filtereinstellungen (Metadaten).",
            br(),
            "Werden Eingaben von Ihnen grundsätzlich für die Durchführung einer Analyse
            benötigt, so sehen Sie dies an entsprechenden roten Hinweisen in den 
            grünen Ergebnisboxen.",
            br(),
            "Die Analyseergebnisse werden erst sichtbar wenn alle notwendigen 
            Eingaben getätigt wurden.",
            br(),
            "Im Ergebnisbereich erscheint außerdem bei vorliegender Analyse 
            ein Button ",
            HTML("<i>Export Excel</i>"),
            HTML(", damit lassen sich <u>alle</u> enthalten Ergebnisse 
                 als einzene Excel-Datei herunterladen."
                 ),
            br(),
            "Die einzelnen Analyseausgaben (Grafik und Tabellen) können außerdem 
            auch einzeln mit Klick auf den entsprechenden Download-Button exportiert werden."
          )
        )
      ),
      # ------------------------------------------------------------------------
      fluidRow(
        column(
          12,
          div(
            id = ns("erweitert"),
            box(
              title = "Erweiterte Einstellungen",
              width = 12,
              solidHeader = FALSE,
              status = "primary",
              collapsible = TRUE, collapsed = FALSE,
              "In den ",
              HTML("<span style='background-color:lightgrey;'>grauen</span>"
                   ),
              " Boxen können weitere Einstellungen getätigt werden, 
              die jedoch nicht unbedingt bei jeder Analyse benötigt werden.",
              br(),
              "Sie können diese Boxen öffnen indem Sie rechts oben auf das Plus-Symbol ",
              HTML("<i class='fas fa-plus'></i>"),
              " klicken und wieder schließen indem Sie auf das Minus-Symbol ",
              HTML("<i class='fas fa-minus'></i>"),
              "klicken.",
              br(),
              HTML("So können Sie auf in der Box <b>Erweiterte Einstellungen Grafik</b>"),
              " (im Ergebnis-Bereich Grafik) verschiedene zusätzliche Einstellungen treffen, 
              welche nur die Grafikausgabe, nicht aber die Analyse an sich betreffen. 
              Das Programm reagiert auf diese Eingaben automatisch. 
              Die Eingabemöglichkeiten unterscheiden sich je nach Grafiktyp. Probieren 
              Sie die verschiedenen Möglichkeiten einfach aus!",
              br(),
              HTML("In der Box <b>Erweiterte Filtereinstellungen</b> können Sie bis zu drei 
                  individuelle Filter auswählen. Im Unterschied zu anderen Eingaben, 
                  reagiert das Programm bei diesen Filtern <u>nicht automatisch</u>. 
                  Das heißt, nach Ihrer Auswahl der Filter betätigen Sie bitte den Button 
                  <i>Filter anwenden</i>, um diese Filter zu aktivieren. Am unteren Rand 
                  der Box ist aufgeführt, welche Filter aktuell aktiv sind. 
                  Betätigen Sie den Button <i>Filter zurücksetzen</i> um die erweiterten 
                  Filter zu löschen und zu deaktivieren."),
              # ----------------------------------------------------------------
              # Erweiterte Filtereinstellungen
              hidden( # nur für Filter-Code
                selectizeInput(ns("jahrFilter"),
                  label = NULL,
                  choices = "Alle Jahre", selected = "Alle Jahre"
                ),
                checkboxInput(ns("wohnMitte"), 
                              label = NULL, value = FALSE),
                selectizeInput(ns("rowVar1"), 
                               label = NULL, choices = "", selected = ""),
                checkboxInput(ns("missValuesRowVar1"), 
                              label = NULL, value = FALSE),
                selectizeInput(ns("colVar"), 
                               label = NULL, choices = "", selected = ""),
                checkboxInput(ns("missValuesColVar1"), 
                              label = NULL, value = FALSE),
                selectizeInput(ns("rowVar2"), 
                               label = NULL, choices = "", selected = ""),
                checkboxInput(ns("missValuesRowVar2"), 
                              label = NULL, value = FALSE)
              ),
              filterDatenUI(id)
            )
          )
        )
      ),
      # ------------------------------------------------------------------------
      fluidRow(
        column(
          12,
          box(
            title = HTML("<span id='saveLoad'>Zurücksetzen, Speichern und Laden von Einstellungen</span>"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                9,
                "Auf den Analyseseiten ist jeweils ein Button zum 
                Zurücksetzen Ihrer Eingaben vorhanden. 
                Damit werden alle Ihre bisherigen Einstellungen gelöscht 
                und die entsprechende Seite wird auf die Starteinstellungen zurückgesetzt."
              ),
              column(
                3,
                actionButton(ns("resetSettings"),
                  "Einstellungen zurücksetzen",
                  icon = icon("undo-alt"),
                  width = "100%"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                9,
                "Auf jeder Analyseseite finden Sie einen Button 
                um Ihre Einstellungen zu speichern.",
                br(),
                HTML(
                  "Nach dem Klick auf diesen Button werden Sie aufgefordert,
                  einen Namen für Ihre Analyseinstellungen einzugeben. 
                  Unter diesem Namen werden die Einstellungen, welche Sie auf der
                  jeweiligen Seite getroffen haben, als .csv-Datei im 
                  Programmordner gespeichert."
                  ),
              "Es werden alle Einstellungen gespeichert - die Auswahl der 
              Analysevariablen, Einstellungen zur Filterführung sowie alle 
              Grafikeinstellungen."
              ),
              column(
                3,
                actionButton(ns("saveSettings"),
                  "Einstellungen speichern",
                  icon = icon("file-export"),
                  width = "100%"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                9,
                "Einstellungen, die Sie auf den Seiten ",
                HTML("<i class='fa fa-chart-line'></i> <b>Zeitreihen Analysen</b>, 
                     <i class='fa fa-chart-bar'></i> <b>Jährliche Analysen</b> oder 
                     <i class='fa fa-table'></i> <b>Individuelle Analysen</b> 
                     gespeichert haben, können Sie auf der Seite 
                     <i class='fa fa-table'></i> <b>Individuelle Analysen</b> 
                     wieder laden."
                     ),
                br(),
                "Einstellungen, die Sie auf der Seite ",
                HTML("<i class='fa fa-map'></i> <b>Karten</b> 
                     gespeichert haben, "
                     ),
                "können Sie direkt dort wieder laden.",
                br(),
                "Dabei werden Sie aufgefordert, aus den Namen der gespeicherten 
                Einstellungen einen auszuwählen.",
                "Wenn Sie Einstellungen laden, werden alle Ihre bisherigen Eingaben 
                auf der entsprechenden Seite zurückgesetzt."
              ),
              column(
                3,
                actionButton(ns("loadSettings"),
                  "Einstellungen laden",
                  icon = icon("file-import"),
                  width = "100%"
                )
              )
            )
          )
        )
      )
    )
  )
}
#
################################################################################
# Module server function:
help <- function(input, output, session) {
  ##############################################################################
  #### Filter : Code einbinden
  ##############################################################################
  source("scripts/module/filterDataServer.R", encoding = "utf8", local = TRUE)
  # resetFilter geklickt: erweiterte Filter zurücksetzen und neu berechnen
  observeEvent(c(input$resetFilter, input$resetSettings), {
    # Filter 1 löschen
    reset("filter1")
    reset("operatorFilter1")
    reset("valueFilter1")
    # enable filter1
    enable("filter1")
    enable("operatorFilter1")
    enable("valueFilter1")
    # Filter 2 löschen
    reset("filter2")
    reset("operatorFilter2")
    reset("valueFilter2")
    # enable filter2
    enable("filter2")
    enable("operatorFilter2")
    enable("valueFilter2")
    # Filter 3 löschen
    reset("filter3")
    reset("operatorFilter3")
    reset("valueFilter3")
    # enable filter3
    enable("filter3")
    enable("operatorFilter3")
    enable("valueFilter3")
    # reaktive Werte löschen
    resetFilterVals()
  })
  # nur für Beispielauswahl
  observeEvent(input$resetSettings, {
    reset("rowVar1")
  })
}
#
################################################################################
