################################################################################
# ESU explorer
#
# global.R
# Ulrike Niemann
#
################################################################################
#
# benötigte Packages (wenn nötig installieren), laden
#
packages <- c(
  "shiny",           # 1.3.2 - shiny
  "shinydashboard",  # 0.7.1 - dashboard
  "shinyjs",         # 1.0 - hidden, reset
  "shinycssloaders", # 0.2.0 - loading wheel
  "tidyverse",       # 1.2.1 - dplyr, ggplot2 ectpp.
  "DT",              # 0.8 - DataTable
  "janitor",         # 1.2.0 - adorn_totals
  "mapproj",         # 1.2.6 - Karten
  "ggiraph",         # 0.7.0 - interaktive Grafiken/Karten
  "Kernelheaping",   # 2.2.1 - Kernelheaping-Verfahren
  "scales",          # 1.1.0 - percent_format
  "Hmisc",           # 4.2.0 - label
  "RColorBrewer",    # 1.1.2 - brewer.pal
  "openxlsx",        # 4.1.0.1 - read.xlsx, write.xlsx
  "DescTools",       # 0.99.28 - ContCoef (CramersV)
  "rlang",           # 0.4.0 - as_name, tidy evaluation
  "officer",         # 0.3.5 - Manipulate Microsoft Documents
  "rvg",             # 0.2.1 - xl_add_vg (Vektorgrafiken)
  "ggExtra"          # 0.9 - ggMarginal
)
packages <- rev(packages)
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, character.only = TRUE)
  }
  library(package, character.only = TRUE)
}
rm(packages, package)
#
################################################################################
# Demo-Version?
DEMO <- TRUE
# Datensatz + Metadaten
if (DEMO == FALSE) {
  load("data/ESUdaten.Rdata")
  meta <- read.xlsx("data/Metadaten_ESUexplorer.xlsx") %>%
    mutate_all(replace_na, "")
} else {
  load("data/ESUdaten_DEMO.Rdata")
  meta <- read.xlsx("data/Metadaten_ESUexplorer_DEMO.xlsx") %>%
    mutate_all(replace_na, "")
}
# Geodaten LOR
load("data/Karten/Mitte_LOR.RData")
# Geodaten ESB
load("data/Karten/ESB2019.RData")
load("data/Karten/ESB2018.RData")
load("data/Karten/ESB2015.RData")
load("data/Karten/ESB2012.RData")
load("data/Karten/ESB2011.RData")
# Geodaten openStreetMap
load("data/Karten/berlinOpenData.RData")
# Schulen Koordinaten
load("data/Karten/Schulen.RData")
#
################################################################################
# globale Variablen/ Inhalte ###################################################
# Gesamtvariable wird benötigt - an erster Stelle im Datensatz -----------------
# Gesamtvariable an den Datensatz anhängen
daten <- daten %>% 
  add_column(Gesamt = factor("Gesamt"), .before = 1) 
attr(daten[["Gesamt"]], "label") <- "Gesamt"
# Gesamtvariable in der Metatabelle definieren
meta <- meta %>%
  add_row(Variable = "Gesamt",
          Bereich = "Gesamt",
          Meßart = "nominal",
          Herkunft = "berechnet: Gesamtvariable",
          Infos = "Variable für eine Gesamt-Auswertung (alle Datensätze; entsprechend der aktiven Filterführung)",
          Anmerkungen = "",
          .before = 1)
# Variablennamen und Variablenlabels abspeichern -------------------------------
varNames <- names(daten) # variablennamen
n <- ncol(daten)
varLabels <- map_chr(1:n, function(x) attr(daten[[x]], "label"))
names(varLabels) <- varNames # Variablenlabels
labelsVector <- paste0(varLabels, " (", varNames, ")")
names(varNames) <- labelsVector
# Variablen in Bereichen (Liste für dropdowns) ---------------------------------
# als erster Bereich wird der für die Zeitvariable zeitVar angenommen!
# welche Bereiche gibt es?
bereiche <- meta %>%
  filter(nchar(Bereich) > 0) %>%
  distinct(Bereich) %>%
  pull()
# eine Liste mit Elementen für jeden Bereich
varsBereiche <- list()
for (i in bereiche) {
  varsBereiche[[ (length(varsBereiche) + 1) ]] <-
    (i <- varNames[varNames %in% meta$Variable[meta$Bereich == i]])
}
names(varsBereiche) <- bereiche
# noch eine Liste diesmal ohne die metrischen Variablen (für maps, facet)
varsBereicheKat <- list()
for (i in bereiche) {
  varsBereicheKat[[ (length(varsBereicheKat) + 1) ]] <-
    (i <- varNames[varNames %in%
      meta$Variable[which(
        meta$Bereich == i &
          meta$Meßart != "metrisch"
      )]])
}
names(varsBereicheKat) <- bereiche
# Levels für die Metadaten -----------------------------------------------------
levelsDf <- varLabels %>%
  as.data.frame() %>%
  set_names("Label") %>%
  rownames_to_column("Variable")
# Anzahl Levels
levelsDf$lengthLevels <- map_int(1:n, function(x) length(levels(daten[[x]])))
# Levels in einem String festhalten
levelsDf$Levels <- map_chr(1:n, function(x) {
  paste(levels(daten[[x]]), collapse = " | ")
})
# an Metadaten anspielen
meta <- left_join(meta, levelsDf, by = "Variable")
#
# Tabelle für MetaInfos DT -----------------------------------------------------
# es werden nur die Vars mit Bereich verwendet
metaTable <- meta %>% filter(Bereich != "")
metaTable <- cbind(
  "Nr" = 1:nrow(metaTable),
  "Variable" = metaTable[, "Variable"],
  "Label" = metaTable[, "Label"],
  metaTable[, 2:5],
  "Levels" = metaTable[, "Levels"]
)
# löschen temporärer Daten -----------------------------------------------------
rm(n)
rm(bereiche)
rm(levelsDf)
rm(i)
#
################################################################################
# Konstanten 
# für den Schließen-Button -----------------------------------------------------
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
# Variable für die Zeit (hier: Schuljahr) --------------------------------------
zeitVar <- "A2q_Schuljahr"
# Variable für den Globalen Filter (hier: Wohnort Bezirksebene)
globalFilterVar <- "FAM_Bezirk"
# Ausprägung für den Globalen Filter (hier: Mitte)
globalFilterLevel <- "Mitte"
# weitere fest genutzte Variablen:
# Vars für kleinräumige Analysen (ESB, BEZ, PRG, BZR, PLR)
# Prozentformat für die Export-Tabellen ----------------------------------------
pctStyle <- createStyle(numFmt = "0.0%")
# goldener Schnitt phi = a/b = (a+b)/a -----------------------------------------
phi <- (1 + sqrt(5)) / 2
#
################################################################################
# Skripte einbinden
# Funktionen
source("scripts/functions.R", encoding = "utf-8")
source("scripts/functionsAnalysis.R", encoding = "utf8")
source("scripts/functionsMaps.R", encoding = "utf8")
source("scripts/functionsKernelheaping.R", encoding = "utf8")
# Module und weitere Skripte
source("scripts/module/analysis.R", encoding = "utf8")
source("scripts/module/maps.R", encoding = "utf8")
source("scripts/module/filterDataUI.R", encoding = "utf8")
source("scripts/module/metaTab.R", encoding = "utf8")
source("scripts/module/downloadDoku.R", encoding = "utf8")
source("scripts/module/help.R", encoding = "utf8")
#
################################################################################
################################################################################
