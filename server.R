################################################################################
# ESU explorer
#
# server.R
# Ulrike Niemann
#
################################################################################
#
server <- function(input, output, session) {
  # nur für DEMO-Version
  if (DEMO == TRUE) {
    showElement(id = "DEMO")
  }
  # Stop App wenn Browser(tab) geschlossen
  session$onSessionEnded(stopApp) 
  # Schließen-Button
  observeEvent(input$close, { 
    js$closeWindow()
    stopApp()
  })
  # Startseite -----------------------------------------------------------------
  observeEvent(input$switchtabZeit, {
    updateTabItems(session, "tabs", "zeitanalyse")
  })
  observeEvent(input$switchtabJahr, {
    updateTabItems(session, "tabs", "jahresanalyse")
  })
  observeEvent(input$switchtabAnalyse, {
    updateTabItems(session, "tabs", "analysen")
  })
  observeEvent(input$switchtabKarte, {
    updateTabItems(session, "tabs", "karten")
  })
  observeEvent(input$switchtabInfo, {
    updateTabItems(session, "tabs", "varinfo")
  })
  observeEvent(input$switchtabDoku, {
    updateTabItems(session, "tabs", "dokubogen")
  })
  observeEvent(input$switchtabHilfe, {
    updateTabItems(session, "tabs", "help")
  })
  observeEvent(input$switchtabAbout, {
    updateTabItems(session, "tabs", "about")
  })
  # Zeitreihen -----------------------------------------------------------------
  callModule(analysen, "zeit")
  # Jährliche Analyse ----------------------------------------------------------
  callModule(analysen, "jahr")
  # Individuelle Analysen ------------------------------------------------------
  callModule(analysen, "analyse")
  # Karte erstellen ------------------------------------------------------------
  callModule(karteErstellen, "karte")
  # Metadaten  -----------------------------------------------------------------
  callModule(metaTab, "metaInfo")
  # Download Dokumentationsbogen -----------------------------------------------
  callModule(downloadBogen, "downloadBogen")
  # Hilfe ----------------------------------------------------------------------
  callModule(help, "help")
}
################################################################################
