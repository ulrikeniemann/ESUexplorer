################################################################################
# ESU explorer
#
# filterDataServer.R (integriert in module "analysis" und "maps")
# Ulrike Niemann
#
################################################################################
#### erweiterte Filter:
################################################################################
# operator sollen immer belegt sein
observeEvent(input$operatorFilter1, {
  if (input$operatorFilter1 == "") {
    reset("operatorFilter1")
  }
})
observeEvent(input$operatorFilter2, {
  if (input$operatorFilter2 == "") {
    reset("operatorFilter2")
  }
})
observeEvent(input$operatorFilter3, {
  if (input$operatorFilter3 == "") {
    reset("operatorFilter3")
  }
})
# ------------------------------------------------------------------------------
#### update selectizeInput Filter 1 - Auswahlkategorien (Level)
observeEvent(input$filter1, {
  if (nzchar(input$filter1)) {
    if (filterSet$filter1 != input$filter1) {
      updateSelectizeInput(session, "valueFilter1",
        choices = levels(daten[[input$filter1]]),
        server = TRUE
      )
    }
  } else { # input ist leer
    reset("operatorFilter1")
    updateSelectizeInput(session, "valueFilter1",
      choices = "",
      server = TRUE
    )
  }
})
#### update selectizeInput Filter 2 - Auswahlkategorien (Level)
observe({
  if (nchar(input$filter2) > 0) {
    # disable filter1
    disable("filter1")
    disable("operatorFilter1")
    disable("valueFilter1")
    # wenn nicht geladen: level aktualisieren
    if (filterSet$filter2 != input$filter2) {
      updateSelectizeInput(session, "valueFilter2",
        choices = levels(daten[[input$filter2]]),
        server = TRUE
      )
    }
  } else { # input ist leer
    # enable filter1
    enable("filter1")
    enable("operatorFilter1")
    enable("valueFilter1")
    # operator und values zurücksetzen
    reset("operatorFilter2")
    updateSelectizeInput(session, "valueFilter2",
      choices = "",
      server = TRUE
    )
  }
})
#### update selectizeInput Filter 3 - Auswahlkategorien (Level)
observe({
  if (nchar(input$filter3) > 0) {
    # disable filter2
    disable("filter2")
    disable("operatorFilter2")
    disable("valueFilter2")
    # wenn nicht geladen: level aktualisieren
    if (filterSet$filter3 != input$filter3) {
      updateSelectizeInput(session, "valueFilter3",
        choices = levels(daten[[input$filter3]]),
        server = TRUE
      )
    }
  } else { # input ist leer
    # enable filter1
    enable("filter2")
    enable("operatorFilter2")
    enable("valueFilter2")
    # operator und values zurücksetzen
    reset("operatorFilter3")
    updateSelectizeInput(session, "valueFilter3",
      choices = "",
      server = TRUE
    )
  }
})
# ------------------------------------------------------------------------------
#### Button zurücksetzen anzeigen nur wenn irgendwas gesetzt
observe({
  # hier muss auf Textinhalt geprüft werden (single input)
  if (input$filter1 != "" || filterSet$filter1 != "" ||
    input$filter2 != "" || filterSet$filter2 != "" ||
    input$filter3 != "" || filterSet$filter3 != "") {
    showElement(id = "resetFilter")
  } else {
    hideElement(id = "resetFilter")
  }
})
# Filterauswahl? reactive values -----------------------------------------------
valuesSelected <- reactiveValues(
  filter1 = NULL,
  filter2 = NULL,
  filter3 = NULL
)
observeEvent(length(input$valueFilter1), {
  if (length(input$valueFilter1) == 0) {
    valuesSelected$filter1 <- FALSE
  } else {
    valuesSelected$filter1 <- TRUE
  }
})
observeEvent(length(input$valueFilter2), {
  if (length(input$valueFilter2) == 0) {
    valuesSelected$filter2 <- FALSE
  } else {
    valuesSelected$filter2 <- TRUE
  }
})
observeEvent(length(input$valueFilter3), {
  if (length(input$valueFilter3) == 0) {
    valuesSelected$filter3 <- FALSE
  } else {
    valuesSelected$filter3 <- TRUE
  }
})
# ------------------------------------------------------------------------------
#### Button Filter anwenden, +2.Filter nur wenn alles (Filter 1) ausgewählt ----
observeEvent(valuesSelected$filter1, {
  if (valuesSelected$filter1 == TRUE) {
    # Button anzeigen
    showElement(id = "filterDo")
    showElement(id = "divDeleteFilter1")
    showElement(id = "divZweitFilter")
  } else {
    # Button verbergen
    hideElement(id = "filterDo")
    hideElement(id = "divDeleteFilter1")
    hideElement(id = "divZweitFilter")
  }
})
#### +3.Filter nur wenn alles (Filter 2) ausgewählt ----------------------------
observe({
  if (valuesSelected$filter2 == TRUE) {
    hideElement(id = "divZweitFilter") # + 2. Filter ausblenden (nur beim Laden relevant)
    hideElement(id = "divDeleteFilter1") # -1. Filter ausblenden -"-
    if (!valuesSelected$filter3) {
      showElement(id = "divDrittFilter") # + 3. Filter
    }
  } else {
    hideElement(id = "divDrittFilter")
  }
})
# - 1. Filter (löschen) klick: -------------------------------------------------
observeEvent(input$deleteFilter1, {
  # Filter 1 löschen
  updateSelectizeInput(session, "filter1", selected = "")
  updateSelectizeInput(session, "operatorFilter1", selected = "gleich")
  updateSelectizeInput(session, "valueFilter1",
    choices = "", server = TRUE
  )
  hideElement(id = "divDeleteFilter1")
  hideElement(id = "divZweitFilter")
})
# + 2. Filter klick: -----------------------------------------------------------
observeEvent(input$zweitFilter, {
  showElement(id = "filter2")
  showElement(id = "operatorFilter2")
  showElement(id = "valueFilter2")
  showElement(id = "divDeleteFilter2")
  hideElement(id = "divDeleteFilter1")
  hideElement(id = "divZweitFilter")
  # disable filter1
  disable("filter1")
  disable("operatorFilter1")
  disable("valueFilter1")
})
# - 2. Filter (löschen) klick: -----------------------------------------------
observeEvent(input$deleteFilter2, {
  # Filter 2 löschen
  updateSelectizeInput(session, "filter2", selected = "")
  updateSelectizeInput(session, "operatorFilter2", selected = "gleich")
  updateSelectizeInput(session, "valueFilter2",
    choices = "", server = TRUE
  )
  hideElement(id = "filter2")
  hideElement(id = "operatorFilter2")
  hideElement(id = "valueFilter2")
  hideElement(id = "divDeleteFilter2")
  hideElement(id = "divDrittFilter")
  showElement(id = "divDeleteFilter1")
  showElement(id = "divZweitFilter")
  # enable filter1
  enable("filter1")
  enable("operatorFilter1")
  enable("valueFilter1")
})
# + 3. Filter klick: ---------------------------------------------------------
observeEvent(input$drittFilter, {
  showElement(id = "filter3")
  showElement(id = "operatorFilter3")
  showElement(id = "valueFilter3")
  showElement(id = "divDeleteFilter3")
  hideElement(id = "divDeleteFilter2")
  hideElement(id = "divDrittFilter")
  # disable filter2
  disable("filter2")
  disable("operatorFilter2")
  disable("valueFilter2")
})
# - 3. Filter (löschen) klick: -----------------------------------------------
observeEvent(input$deleteFilter3, {
  # Filter 3 löschen
  updateSelectizeInput(session, "filter3", selected = "")
  updateSelectizeInput(session, "operatorFilter3", selected = "gleich")
  updateSelectizeInput(session, "valueFilter3",
    choices = "", server = TRUE
  )
  hideElement(id = "filter3")
  hideElement(id = "operatorFilter3")
  hideElement(id = "valueFilter3")
  hideElement(id = "divDeleteFilter3")
  showElement(id = "divDeleteFilter2")
  showElement(id = "divDrittFilter")
  # enable filter2
  enable("filter2")
  enable("operatorFilter2")
  enable("valueFilter2")
})
################################################################################
# Filter anwenden: Button
################################################################################
# Eingaben als reactiveValues speichern
filterSet <- reactiveValues(
  filter1 = "",
  operatorFilter1 = "",
  valueFilter1 = "",
  filter2 = "",
  operatorFilter2 = "",
  valueFilter2 = "",
  filter3 = "",
  operatorFilter3 = "",
  valueFilter3 = "",
  filterALLText = ""
)
# Filter Werte zurücksetzen ----------------------------------------------------
resetFilterVals <- function() {
  filterSet$filter1 <- ""
  filterSet$operatorFilter1 <- ""
  filterSet$valueFilter1 <- ""
  filterSet$filter2 <- ""
  filterSet$operatorFilter2 <- ""
  filterSet$valueFilter2 <- ""
  filterSet$filter3 <- ""
  filterSet$operatorFilter3 <- ""
  filterSet$valueFilter3 <- ""
  filterSet$filterALLText <- ""
  # Filteranzeigen zurücksetzen
  hideElement(id = "filter2")
  hideElement(id = "operatorFilter2")
  hideElement(id = "valueFilter2")
  hideElement(id = "divDeleteFilter2")
  hideElement(id = "divDrittFilter")
  hideElement(id = "filter3")
  hideElement(id = "operatorFilter3")
  hideElement(id = "valueFilter3")
  hideElement(id = "divDeleteFilter3")
}
# ------------------------------------------------------------------------------
varLabel <- function(var) {
  which(varNames == var) %>% names()
}
filterVarText <- function(varLabel, operator, value) {
  paste0(
    varLabel, " <i>",
    operator, "</i> ",
    paste(value, collapse = ", ")
  )
}
# Button Filter angewenden klick: reactiveValues besetzen, Infos anzeigen ------
observeEvent(input$filterDo, {
  # Filter 1
  if (length(input$valueFilter1) > 0) {
    filterSet$filter1 <- input$filter1
    filterSet$operatorFilter1 <- input$operatorFilter1
    filterSet$valueFilter1 <- input$valueFilter1
    filterSet$filterALLText <- filterVarText(
      varLabel(filterSet$filter1),
      filterSet$operatorFilter1,
      filterSet$valueFilter1
    )
  } else {
    filterSet$filter1 <- ""
    filterSet$operatorFilter1 <- ""
    filterSet$valueFilter1 <- ""
    filterSet$filterALLText <- ""
  }
  # Filter 2
  if (length(input$valueFilter2) > 0) {
    filterSet$filter2 <- input$filter2
    filterSet$operatorFilter2 <- input$operatorFilter2
    filterSet$valueFilter2 <- input$valueFilter2
    filterSet$filterALLText <- paste0(
      filterSet$filterALLText, "<br>",
      filterVarText(
        varLabel(filterSet$filter2),
        filterSet$operatorFilter2,
        filterSet$valueFilter2
      )
    )
  } else {
    filterSet$filter2 <- ""
    filterSet$operatorFilter2 <- ""
    filterSet$valueFilter2 <- ""
  }
  # Filter 3
  if (length(input$valueFilter3) > 0) {
    filterSet$filter3 <- input$filter3
    filterSet$operatorFilter3 <- input$operatorFilter3
    filterSet$valueFilter3 <- input$valueFilter3
    filterSet$filterALLText <- paste0(
      filterSet$filterALLText, "<br>",
      filterVarText(
        varLabel(filterSet$filter3),
        filterSet$operatorFilter3,
        filterSet$valueFilter3
      )
    )
  } else {
    filterSet$filter3 <- ""
    filterSet$operatorFilter3 <- ""
    filterSet$valueFilter3 <- ""
  }
}, priority = 20)
# Button resetFilter: alles zurücksetzen ---------------------------------------
# siehe module observeEvent(input$resetFilter
# ----------------------------------------------------------------------------
# Filter Textausgabe insgesamt erstellen (globale + individuelle Filter)
filterText <- reactive({
  text <- paste0("<b>Aktive Filter:</b><br>")
  if (input$wohnMitte == TRUE) {
    text <- paste0(
      text,
      varLabel(globalFilterVar),
      " <i>gleich</i> ", globalFilterLevel, "<br>"
    )
  }
  if (input$jahrFilter != "Alle Jahre") {
    text <- paste0(
      text, varLabel(zeitVar),
      " <i>gleich</i> ", input$jahrFilter, "<br>"
    )
  }
  if (input$rowVar1 != "" && input$missValuesRowVar1 == TRUE) {
    text <- paste0(
      text, varLabel(input$rowVar1),
      " <i>ungleich</i> (fehlende Werte)<br>"
    )
  }
  if (input$colVar1 != "" && input$missValuesColVar1 == TRUE) {
    text <- paste0(
      text, varLabel(input$colVar1),
      " <i>ungleich</i> (fehlende Werte)<br>"
    )
  }
  if (input$rowVar2 != "" && input$missValuesRowVar2 == TRUE) {
    text <- paste0(
      text, varLabel(input$rowVar2),
      " <i>ungleich</i> (fehlende Werte)<br>"
    )
  }
  # Text individuelle Filter
  if (filterSet$filterALLText != "") {
    text <- paste0(
      text,
      "<b>Erweiterte aktive Filter:</b> <br>",
      filterSet$filterALLText
    )
  }
  # gar kein Filter?
  if (nchar(text) < 26) {
    text <- paste0(text, "<i>keine aktiven Filter!</i>")
  }
  return(text)
})
# infoFilter HTML Output -------------------------------------------------------
output$infoFilter <- renderUI({
  HTML(filterText())
})
# # Tabellenausgabe Filter -----------------------------------------------------
filterTab <- reactive({
  filterInfo <- data.frame(
    Variable = character(),
    Operator = character(),
    Level = character(),
    stringsAsFactors = FALSE
  )
  # Globale Filter -------------------------------------------------------------
  if (input$wohnMitte == TRUE) {
    filterInfo <- filterInfo %>%
      add_row(
        Variable = varLabel(globalFilterVar),
        Operator = "gleich", Level = globalFilterLevel
      )
  }
  if (input$jahrFilter != "Alle Jahre") {
    filterInfo <- filterInfo %>%
      add_row(
        Variable = varLabel(zeitVar),
        Operator = "gleich", Level = input$jahrFilter
      )
  }
  if (input$rowVar1 != "" && input$missValuesRowVar1 == TRUE) {
    filterInfo <- filterInfo %>%
      add_row(
        Variable = varLabel(input$rowVar1),
        Operator = "ungleich", Level = "(fehlende Werte)"
      )
  }
  if (input$colVar1 != "" && input$missValuesColVar1 == TRUE) {
    filterInfo <- filterInfo %>%
      add_row(
        Variable = varLabel(input$colVar1),
        Operator = "ungleich", Level = "(fehlende Werte)"
      )
  }
  if (input$rowVar2 != "" && input$missValuesRowVar2 == TRUE) {
    filterInfo <- filterInfo %>%
      add_row(
        Variable = varLabel(input$rowVar2),
        Operator = "ungleich", Level = "(fehlende Werte)"
      )
  }
  # Erweiterte Filter ----------------------------------------------------------
  if (filterSet$filter1 != "") {
    filterInfo <- filterInfo %>% add_row(
      Variable = varLabel(filterSet$filter1),
      Operator = filterSet$operatorFilter1,
      Level = paste(filterSet$valueFilter1,
        collapse = ", "
      )
    )
  }
  if (filterSet$filter2 != "") {
    filterInfo <- filterInfo %>% add_row(
      Variable = varLabel(filterSet$filter2),
      Operator = filterSet$operatorFilter2,
      Level = paste(filterSet$valueFilter2,
        collapse = ", "
      )
    )
  }
  if (filterSet$filter3 != "") {
    filterInfo <- filterInfo %>% add_row(
      Variable = varLabel(filterSet$filter3),
      Operator = filterSet$operatorFilter3,
      Level = paste(filterSet$valueFilter3,
        collapse = ", "
      )
    )
  }
  # gar keine Filter -----------------------------------------------------------
  if (nrow(filterInfo) == 0) {
    filterInfo <- filterInfo %>% add_row(Variable = "keine aktiven Filter!")
  }
  return(filterInfo)
})
################################################################################