################################################################################
# ESU explorer
#
# ui.R
# Ulrike Niemann
#
################################################################################
#
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      icon("search"),
      tags$span("ESU explorer",
        class = "ESUexplorer",
        style = "font-size: 24px;"
      )
    ),
    # nur für die DEMO-Version
    tags$li(
      class = "dropdown",
      hidden(div(id = "DEMO",
           "DEMO-Version! Simulierte Daten - nur zum Test der Funktionalitäten!"))
    ),
    tags$li(
      class = "dropdown",
      span(
        id = "divClose",
        title = "Anwendung schließen",
        actionButton("close",
          label =
            HTML("<i class='far fa-window-close'></i>")
        )
      )
    )
  ),
  ##############################################################################
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Einstieg",
               tabName = "dashboard",
               icon = icon("dashboard")
      ),
      # ------------------------------------------------------------------------
      div("Analysen:", class = "sidebar_text"),
      menuItem("Zeitreihen Analyse",
               tabName = "zeitanalyse",
               icon = icon("chart-line")
      ),
      menuItem("Jährliche Analyse",
               tabName = "jahresanalyse",
               icon = icon("chart-bar")
      ),
      menuItem("Individuelle Analyse",
               tabName = "analysen",
               icon = icon("table")
      ),
      menuItem("Karten",
               tabName = "karten",
               icon = icon("map")
      ),
      # ------------------------------------------------------------------------
      div("Metadaten:", class = "sidebar_text"),
      menuItem("Variablen Infos",
               tabName = "varinfo",
               icon = icon("info-circle")
      ),
      menuItem("Dokumentationsbögen",
               tabName = "dokubogen",
               icon = icon("file-pdf")
      ),
      menuItem("Hilfe",
               tabName = "help",
               icon = icon("question-circle")
      ),
      menuItem("Über",
               tabName = "about",
               icon = icon("info")
      )
    )
  ),
  ##############################################################################
  dashboardBody(
    # shinyjs einbinden
    useShinyjs(),
    # jscode siehe global.R, Schließen-Button
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    # CSS stylesheet einbinden
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "ESUexplorer.css"
      )
    ),
    tabItems(
      # ------------------------------------------------------------------------
      # Einstieg
      tabItem(
        tabName = "dashboard",
        tags$span("Willkommen beim ", style = "font-size: 30px;"),
        tags$span(icon("search"), style = "font-size: 30px;"),
        tags$span("ESU explorer", 
                  style = "font-size: 30px;", 
                  class = "ESUexplorer"),
        tags$hr(style = "border-color: #367fa9;"),
        h4("Analysieren Sie die Daten der Einschulungsuntersuchung (ESU) 
               der Schuljahre 2010 - 2019."),
        tags$hr(style = "border-color: #367fa9;"),
        fluidRow(
          valueBox(
            HTML("Analysieren Sie die Ergebnisse<br>im <b>Zeitverlauf</b>."),
            actionButton("switchtabZeit", "Zeitreihen Analyse"),
            icon = icon("chart-line"),
            color = "light-blue",
            width = 6
          ),
          valueBox(
            HTML("Analysieren Sie die Ergebnisse<br>eines <b>Schuljahres</b>."),
            actionButton("switchtabJahr", "Jährliche Analyse"),
            icon = icon("chart-bar"),
            color = "light-blue",
            width = 6
          )
        ),
        fluidRow(
          valueBox(
            HTML("Analysieren Sie die Ergebnisse<br>ganz <b>individuell</b>."),
            actionButton("switchtabAnalyse", "Individuelle Analyse"),
            icon = icon("table"),
            color = "light-blue",
            width = 6
          ),
          valueBox(
            HTML("Erstellen Sie die kleinräumige<br>Analysen und <b>Karten</b>."),
            actionButton("switchtabKarte", "Karten"),
            icon = icon("map"),
            color = "purple",
            width = 6
          )
        ),
        tags$hr(style = "border-color: #367fa9;"),
        h4("Erhalten Sie Zugriff auf Metadaten und weitere Informationen zur Anwendung."),
        tags$hr(style = "border-color: #367fa9;"),
        fluidRow(
          valueBox(
            HTML("Suchen Sie Variablen und erhalten Sie <br><b>detaillierte Informationen</b> zu diesen."),
            actionButton("switchtabInfo", "Variablen Informationen"),
            icon = icon("info-circle"),
            color = "olive",
            width = 6
          ),
          valueBox(
            HTML("Erhalten Sie die <b>Dokumentationsbögen</b><br>für jedes Schuljahr."),
            actionButton("switchtabDoku", "Dokumentationsbögen"),
            icon = icon("file-pdf"),
            color = "olive",
            width = 6
          )
        ),
        fluidRow(
          valueBox(
            HTML("Besuchen Sie den <b>Hilfebereich</b><br>für diese Anwendung."),
            actionButton("switchtabHilfe", "Hilfe"),
            icon = icon("question-circle"),
            color = "olive",
            width = 6
          ),
          valueBox(
            HTML("Erhalten Sie Informationen<br>über die Anwendung."),
            actionButton("switchtabAbout", "Über den ESU explorer"),
            icon = icon("info"),
            color = "olive",
            width = 6
          )
        ),
        tags$hr(style = "border-color: #367fa9;")
      ),
      # ------------------------------------------------------------------------
      # tab: Zeitreihen
      tabItem(
        tabName = "zeitanalyse",
        h2("Zeitreihen Analyse"),
        analysenUI("zeit")
      ),
      # ------------------------------------------------------------------------
      # tab: Jahresanalyse
      tabItem(
        tabName = "jahresanalyse",
        h2(enc2utf8("Jährliche Analyse")),
        analysenUI("jahr")
      ),
      # ------------------------------------------------------------------------
      # tab: Individuelle Analyse
      tabItem(
        tabName = "analysen",
        h2("Individuelle Analyse"),
        analysenUI("analyse")
      ),
      # ------------------------------------------------------------------------
      # tab: Karten
      tabItem(
        tabName = "karten",
        h2(enc2utf8("Karten: Kleinräumige Analyse")),
        karteErstellenUI("karte")
      ),
      # ------------------------------------------------------------------------
      # tab: varinfo
      tabItem(
        tabName = "varinfo",
        h2("Datensatz und Variablen Informationen (Metadaten)"),
        metaTabUI("metaInfo")
      ),
      # ------------------------------------------------------------------------
      # tab: dokubogen
      tabItem(
        tabName = "dokubogen",
        h2(enc2utf8("Dokumentationsbögen 2010 - 2019")),
        downloadBogenUI("downloadBogen")
      ),
      # ------------------------------------------------------------------------
      # tab: Hilfe
      tabItem(
        tabName = "help",
        h2(enc2utf8("Hilfebereich")),
        helpUI("help")
      ),
      # ------------------------------------------------------------------------
      # tab: about
      tabItem(
        tabName = "about",
        h2(
          "Über den ",
          tags$span("ESU explorer", 
                    style = "font-size: 30px;", 
                    class = "ESUexplorer")
        ),
        tagList(
          br(), "Der ",
          tags$span("ESU explorer", 
                    style = "font-size: 16px;", class = "ESUexplorer"),
          "  wurde entwickelt von:", br(), br(),
          "Ulrike Niemann", br(),
          HTML("<a href='mailto:ulrike.niemann@e-market-research.de'>
                   ulrike.niemann@e-market-research.de</a>"),
          br(), br(),
          "im Rahmen einer Masterarbeit zum Thema",
          HTML("<br><br><b>Explorative Datenanalyse mit R-Shiny:<br> 
                   Eine Anwendungsentwicklung im Rahmen des 
                   Bildungsmonitoring Berlin-Mitte.</b>"),
          br(), br(),
          "für die Nutzung im Bezirksamt Mitte von Berlin.", br(), br(),
          img(src = "logo-ba-mitte.jpg")
        )
      )
      # ------------------------------------------------------------------------
    )
  ),
  ##############################################################################
  title = "ESU explorer"
)
#
################################################################################
