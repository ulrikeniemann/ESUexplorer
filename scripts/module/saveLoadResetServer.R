################################################################################
# ESU explorer
#
# saveLoadResetServer (integriert in module "analysis" und "maps")
# Ulrike Niemann
#
################################################################################
# Einstellungen speichern und laden:
# primäres Verzeichnis festlegen
outputDirAnalysen <- "settings/Analysen"
outputDirKarten <- "settings/Karten"
# Definition der Felder die gespeichert werden sollen
fieldsAnalyse <- c(
  "rowVar1", "missValuesRowVar1",
  "colVar1", "missValuesColVar1",
  "rowVar2", "missValuesRowVar2",
  "wohnMitte", "jahrFilter",
  "filter1", "operatorFilter1", "valueFilter1",
  "filter2", "operatorFilter2", "valueFilter2",
  "filter3", "operatorFilter3", "valueFilter3",
  "plotType",
  "labelsHidePlot",
  "hidePercentPlot",
  "percentHide",
  "geomTextSize",
  "baseSize",
  "legende",
  "color",
  "aspectRatio",
  "trend",
  "rowHide", # multiple, extra Akt.
  "colHide", # multiple, extra Akt.
  "rowHide2", # multiple, extra Akt.
  "boxplot", "violin", "mw",
  "bins",
  "density"
)
fieldsKarte <- c(
  "raum", "jahrFilter",
  "rowVar1", "missValuesRowVar1",
  "level", # abh. von rowVar1 UND mydata, extra Akt.
  "filter1", "operatorFilter1", "valueFilter1",
  "filter2", "operatorFilter2", "valueFilter2",
  "filter3", "operatorFilter3", "valueFilter3",
  "valueShow",
  "percentHide",
  "geomTextSize",
  "scale",
  "scaleNumber",
  "quantileNumber",
  "baseSize",
  "legende",
  "color",
  "path", "schulen", "water", "green", "other",
  "legende2",
  "color2",
  "path2", "schulen2", "water2", "green2", "other2"
)
################################################################################
### Einstellungen speichern
################################################################################
# modal: Name für die Auswertung angeben ---------------------------------------
saveModal <- function(nameEmpty = FALSE) {
  ns <- session$ns
  modalDialog(
    textInput(ns("settingsName"), "Name für das Speichern der Einstellungen",
      placeholder = "Name"
    ),
    span(HTML(
      "Bitte geben Sie einen Namen für diese Auswertung an!<br>",
      "(Die Einstellungen werden als .csv-Datei abgelegt.)"
    )),
    if (nameEmpty) {
      div(tags$b("Bitte geben Sie einen Namen an.", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Abbrechen"),
      actionButton(ns("okSaveSettings"), "OK")
    )
  )
}
# modal: wenn vorhanden, überschreiben? ----------------------------------------
overwriteSettings <- function(fileName) {
  ns <- session$ns
  modalDialog(
    title = div(tags$b("Name vorhanden!", style = "color: red;")),
    HTML(
      "Unter dem Namen <br><i>",
      fileName,
      "<br></i>ist bereits eine Auswertung gespeichert.<br>
      Sollen die Einstellungen unter diesem Namen gespeichert werden?<br>
      Die vorhandenen Daten werden dann überschrieben."
    ),
    footer = tagList(
      modalButton("Abbrechen"),
      actionButton(
        ns("newNameSettings"),
        "Nein, neuen Namen eingeben"
      ),
      actionButton(
        ns("overwriteSettings"),
        "Ja, unter diesem Namen speichern"
      )
    )
  )
}
# Button Einstellungen speichern -----------------------------------------------
observeEvent(input$saveSettings, {
  showModal(saveModal())
})
writeSettings <- function(outputDir, fileName) {
  if (session$ns("") != "karte-") {
    outputDir <- outputDirAnalysen
    fields <- fieldsAnalyse
  } else {
    outputDir <- outputDirKarten
    fields <- fieldsKarte
  }
  data <- paste0(
    fields, ";",
    sapply(fields, function(inpt) input[[inpt]])
  )
  write(data, file.path(outputDir, fileName))
}
# saveModal: OK Button ---------------------------------------------------------
fileName <- reactiveValues(txt = NULL, readdFile = NULL)
observeEvent(input$okSaveSettings, {
  if (session$ns("") != "karte-") {
    outputDir <- outputDirAnalysen
  } else {
    outputDir <- outputDirKarten
  }
  fileName$txt <- input$settingsName
  # gültiger Name?
  fileName <- paste0(input$settingsName, ".csv")
  if (!is.null(input$settingsName) && nzchar(input$settingsName) &&
    !file.exists(file.path(outputDir, fileName))) {
    writeSettings(outputDir, fileName)
    removeModal()
  } else if (file.exists(file.path(outputDir, fileName))) {
    # unter diesem Namen ist bereits etwas abgespeichert
    showModal(overwriteSettings(input$settingsName))
  } else {
    # kein/falscher name angegeben
    showModal(saveModal(nameEmpty = TRUE))
  }
})
# overwriteSettings: Button neuen Namen angeben --------------------------------
observeEvent(input$newNameSettings, {
  showModal(saveModal())
})
# overwriteSettings: Button Auswertung überschreiben ---------------------------
observeEvent(input$overwriteSettings, {
  if (session$ns("") != "karte-") {
    outputDir <- outputDirAnalysen
  } else {
    outputDir <- outputDirKarten
  }
  fileName <- paste0(fileName$txt, ".csv")
  writeSettings(outputDir, fileName)
  removeModal()
})
################################################################################
### Einstellungen zurücksetzen
################################################################################
resetForm <- function() {
  reset("form") # inputs zurücksetzen
  myDataSet$data <- NULL # Daten zurücksetzen
  resetFilterVals() # Filterwerte zurücksetzen
}
observeEvent(input$resetSettings, {
  resetForm()
}, priority = 20)
################################################################################
### Einstellungen laden
################################################################################
# Button Einstellungen laden ---------------------------------------------------
observeEvent(input$loadSettings, {
  showModal(loadModal())
})
# modal Einstellungen laden: Auswertung auswählen ------------------------------
loadModal <- function(nameEmpty = FALSE) {
  ns <- session$ns
  if (session$ns("") != "karte-") {
    outputDir <- outputDirAnalysen
  } else {
    outputDir <- outputDirKarten
  }
  files <- list.files(outputDir,
    full.names = FALSE, pattern = ".csv"
  )
  resetForm() # erst mal alles zurücksetzen
  modalDialog(
    selectizeInput(ns("loadName"),
      label = "Auswertung auswählen",
      choices = files
    ),
    span(HTML("Bitte wählen Sie eine gespeicherte Auswertung.<br>
                Alle bisherigen Einstellungen werden zurückgesetzt.")),
    if (nameEmpty) {
      div(tags$b("Bitte wählen Sie eine gespeicherte Auswertung aus.",
        style = "color: red;"
      ))
    },
    footer = tagList(
      modalButton("Abbrechen"),
      actionButton(ns("okLoadSettings"), "OK")
    )
  )
}
# saveModal: OK Button ---------------------------------------------------------
observeEvent(input$okLoadSettings, {
  if (session$ns("") != "karte-") {
    outputDir <- outputDirAnalysen
  } else {
    outputDir <- outputDirKarten
  }
  # gültiger Name?
  if (!is.null(input$loadName) && nzchar(input$loadName) &&
    file.exists(file.path(outputDir, input$loadName))) {
    inputData <- importFile() # csv-File importieren
    loadInputData(inputData) # inputs neu setzen + Fehlerbehandlung
    removeModal()
  } else {
    # Name leer oder nicht vorhanden
    showModal(loadModal(nameEmpty = TRUE))
  }
}, priority = 15)
# Inputs neu besetzen ##########################################################
hideSet <- reactiveValues(
  valuesRowHide = "",
  valuesColHide = "",
  valuesRowHide2 = ""
)
levelSet <- reactiveValues(levelVar = "")
#
loadInputData <- function(inputData) {
  # pauschal alles durchgehen (multiple inputs funktioniert nicht) -------------
  for (row in 1:nrow(inputData)) {
    if (inputData[row, 2] == "TRUE" || inputData[row, 2] == "FALSE") {
      # für die Checkboxen müssen die Werte als logical vorliegen
      val <- as.logical(inputData[row, 2])
    } else {
      val <- inputData[row, 2]
    }
    session$sendInputMessage(inputData[row, 1], list(value = val))
  }
  # Werte für ein bestimmtes Feld abfragen -------------------------------------
  fieldValue <- function(field) {
    value <- inputData[which(inputData[, 1] == field), 2]
    # wenn nicht gespeichert dann passiert auch nichts:
    if (is.empty(value)) {
      value <- ""
    }
    return(value)
  }
  # beim plotType erst mal alles zulassen, damit gewählt werden kann -----------
  updateSelectizeInput(session, "plotType",
    choices = c(choicesNominal, choicesMetrisch),
    selected = fieldValue("plotType")
  )
  # jetzt die multiplen inputs -------------------------------------------------
  # Filter 1 - wenn vorhanden: Werte aktualisieren -----------------------------
  if (fieldValue("filter1") != "") {
    # reactiveVals besetzen
    filterSet$filter1 <- fieldValue("filter1")
    filterSet$operatorFilter1 <-
      fieldValue("operatorFilter1")
    # je nach dem ob mehrere oder nur eine Ausprägung gesetzt ist
    if (str_sub(fieldValue("valueFilter1"), 1, 3) ==
      "c(\"") {
      filterSet$valueFilter1 <-
        parse(text = fieldValue("valueFilter1")) %>%
        eval() %>%
        as.character()
    } else {
      filterSet$valueFilter1 <- fieldValue("valueFilter1")
    }
    filterSet$filterALLText <- filterVarText(
      varLabel(filterSet$filter1),
      filterSet$operatorFilter1,
      filterSet$valueFilter1
    )
    # die Ausprägungen setzen
    updateSelectizeInput(session, "valueFilter1",
      choices = levels(daten[[filterSet$filter1]]),
      selected = filterSet$valueFilter1,
      server = TRUE
    )
  }
  # Filter 2 - wenn vorhanden: Werte aktualisieren -----------------------------
  if (fieldValue("filter2") != "") {
    # reactiveVals besetzen
    filterSet$filter2 <- fieldValue("filter2")
    filterSet$operatorFilter2 <-
      fieldValue("operatorFilter2")
    # je nach dem ob mehrere oder nur eine Ausprägung gesetzt ist
    if (str_sub(fieldValue("valueFilter2"), 1, 3) ==
      "c(\"") {
      filterSet$valueFilter2 <-
        parse(text = fieldValue("valueFilter2")) %>%
        eval() %>%
        as.character()
    } else {
      filterSet$valueFilter2 <- fieldValue("valueFilter2")
    }
    filterSet$filterALLText <- paste0(
      filterSet$filterALLText, "<br>",
      filterVarText(
        varLabel(filterSet$filter2),
        filterSet$operatorFilter2,
        filterSet$valueFilter2
      )
    )
    # Ausprägungen setzen
    updateSelectizeInput(session, "valueFilter2",
      choices = levels(daten[[filterSet$filter2]]),
      selected = filterSet$valueFilter2,
      server = TRUE
    )
    showElement(id = "filter2")
    showElement(id = "operatorFilter2")
    showElement(id = "valueFilter2")
    showElement(id = "divDeleteFilter2")
    hideElement(id = "divDeleteFilter1")
    hideElement(id = "divZweitFilter")
  }
  # Filter 3 - wenn vorhanden: Werte aktualisieren -----------------------------
  if (fieldValue("filter3") != "") {
    # reactiveVals besetzen
    filterSet$filter3 <- fieldValue("filter3")
    filterSet$operatorFilter3 <-
      fieldValue("operatorFilter3")
    # je nach dem ob mehrere oder nur eine Ausprägung gesetzt ist
    if (str_sub(fieldValue("valueFilter3"), 1, 3) ==
      "c(\"") {
      filterSet$valueFilter3 <-
        parse(text = fieldValue("valueFilter3")) %>%
        eval() %>%
        as.character()
    } else {
      filterSet$valueFilter3 <- fieldValue("valueFilter3")
    }
    filterSet$filterALLText <- paste0(
      filterSet$filterALLText, "<br>",
      filterVarText(
        varLabel(filterSet$filter3),
        filterSet$operatorFilter3,
        filterSet$valueFilter3
      )
    )
    # die Ausprägungen setzen
    updateSelectizeInput(session, "valueFilter3",
      choices = levels(daten[[filterSet$filter3]]),
      selected = filterSet$valueFilter3,
      server = TRUE
    )
    showElement(id = "filter3")
    showElement(id = "operatorFilter3")
    showElement(id = "valueFilter3")
    showElement(id = "divDeleteFilter3")
    hideElement(id = "divDeleteFilter2")
    hideElement(id = "divDrittFilter")
  }
  ##############################################################################
  # Grafikeinstellungen Analysen -----------------------------------------------
  # hier mit reactiveValues, die werden oben im observer genutzt
  # je nach dem ob mehrere oder nur eine Ausprägung gesetzt ist
  # Ausprägungen Zeilenvariable ausblenden -------------------------------------
  if (fieldValue("rowHide") != "") {
    if (str_sub(fieldValue("rowHide"), 1, 3) ==
      "c(\"") {
      hideSet$valuesRowHide <-
        parse(text = fieldValue("rowHide")) %>%
        eval() %>%
        as.character()
    } else {
      hideSet$valuesRowHide <- fieldValue("rowHide")
    }
  }
  # Ausprägungen Spaltenvariable ausblenden ------------------------------------
  if (fieldValue("colHide") != "") {
    if (str_sub(fieldValue("colHide"), 1, 3) ==
      "c(\"") {
      hideSet$valuesColHide <-
        parse(text = fieldValue("colHide")) %>%
        eval() %>%
        as.character()
    } else {
      hideSet$valuesColHide <- fieldValue("colHide")
    }
  }
  # Ausprägungen Unterteilungvariable ausblenden -------------------------------
  if (fieldValue("rowHide2") != "") {
    if (str_sub(fieldValue("rowHide2"), 1, 3) ==
      "c(\"") {
      hideSet$valuesRowHide2 <-
        parse(text = fieldValue("rowHide2")) %>%
        eval() %>%
        as.character()
    } else {
      hideSet$valuesRowHide2 <- fieldValue("rowHide2")
    }
  }
  ##############################################################################
  # Ausprägung Analysevariable bei Karten --------------------------------------
  if (fieldValue("level") != "") {
    levelSet$levelVar <- fieldValue("level")
    choices <- levels(daten[[fieldValue("rowVar1")]])
    # Ausprägungen aktualisieren
    updateSelectizeInput(session, "level",
      choices = choices,
      selected = levelSet$levelVar,
      server = TRUE
    )
  }
}
################################################################################
# Einstellungen aus einer Datei
importFile <- reactive({
  if (session$ns("") != "karte-") {
    outputDir <- outputDirAnalysen
  } else {
    outputDir <- outputDirKarten
  }
  inFile <- paste(outputDir, input$loadName, sep = "/")
  out <- read.csv2(inFile,
    header = FALSE, stringsAsFactors = FALSE,
    quote = ""
  )
  return(out)
})
##############################################################################
