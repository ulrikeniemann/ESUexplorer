################################################################################
# ESU explorer
#
# functions.R
# Ulrike Niemann
#
################################################################################
# Datenbasis für alle Tabellen und Grafiken : zuerst die Daten filtern
dataFilter <- function(colVar1, missValuesColVar1,
                       rowVar1, missValuesRowVar1,
                       rowVar2, missValuesRowVar2,
                       wohnMitte,
                       jahrFilter,
                       filter1 = "", operator1 = "", value1 = "",
                       filter2 = "", operator2 = "", value2 = "",
                       filter3 = "", operator3 = "", value3 = "") {
  # tidy evaluation ------------------------------------------------------------
  rowVar1 <- sym(rowVar1)
  zeitVar <- sym(zeitVar)
  globalFilterVar <- sym(globalFilterVar)
  if (colVar1 != "") colVar1 <- sym(colVar1)
  if (rowVar2 != "") rowVar2 <- sym(rowVar2)
  if (filter1 != "") filter1 <- sym(filter1)
  if (filter2 != "") filter2 <- sym(filter2)
  if (filter3 != "") filter3 <- sym(filter3)
  #
  data <- daten
  # globaler Filter: Kinder wohnhaft in Berlin ---------------------------------
  if (wohnMitte == TRUE) {
    data <- data %>%
      filter(!!globalFilterVar == globalFilterLevel)
  }
  # fehlende Werte Zeilen ------------------------------------------------------
  if (missValuesRowVar1 == TRUE) {
    data <- data %>%
      filter(!!rowVar1 != "(fehlende Werte)")
  }
  # fehlende Werte Spalten -----------------------------------------------------
  if (missValuesColVar1 == TRUE) {
    data <- data %>%
      filter(!!colVar1 != "(fehlende Werte)")
  }
  # fehlende Werte Unterteilung ------------------------------------------------
  if (rowVar2 != "") {
    if (missValuesRowVar2 == TRUE) {
      data <- data %>%
        filter(!!rowVar2 != "(fehlende Werte)")
    }
  }
  # Filter Schuljahr -----------------------------------------------------------
  if (jahrFilter != "Alle Jahre") {
    data <- data %>%
      filter(!!zeitVar == jahrFilter)
  }
  # individueller Filter 1 -----------------------------------------------------
  if (filter1 != "" && operator1 != "" && length(value1) > 0) {
    if (operator1 == "gleich") {
      data <- data %>%
        filter(!!filter1 %in% value1)
    }
    if (operator1 == "ungleich") {
      data <- data %>%
        filter(!(!!filter1 %in% value1))
    }
  }
  # individueller Filter 2 -----------------------------------------------------
  if (filter2 != "" && operator2 != "" && length(value2) > 0) {
    if (operator2 == "gleich") {
      data <- data %>%
        filter(!!filter2 %in% value2)
    }
    if (operator2 == "ungleich") {
      data <- data %>%
        filter(!(!!filter2 %in% value2))
    }
  }
  # individueller Filter 3 -----------------------------------------------------
  if (filter3 != "" && operator3 != "" && length(value3) > 0) {
    if (operator3 == "gleich") {
      data <- data %>%
        filter(!!filter3 %in% value3)
    }
    if (operator3 == "ungleich") {
      data <- data %>%
        filter(!(!!filter3 %in% value3))
    }
  }
  return(data)
}
################################################################################
# Info Tabellen für jede verwendete Variable erzeugen
createInfoTab <- function(var) {
  infos <- data.frame(
    Variable = character(), Info = character(),
    stringsAsFactors = FALSE
  )
  infos <- infos %>%
    add_row(Variable = "Name: ", Info = var) %>%
    add_row(Variable = "Label: ", Info = varLabels[var]) %>%
    add_row(
      Variable = "Meßniveau: ",
      Info = meta[meta$Variable == var, "Meßart"]
    ) %>%
    add_row(Variable = "Infos: ", Info = meta[meta$Variable == var, "Infos"])
}
################################################################################
