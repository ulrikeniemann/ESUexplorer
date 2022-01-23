
################################################################################
#
# SPSS-Datensatz in R Importieren - für den ESU explorer
# (aufbereitet mit SPSS Version 23)
# Ulrike Niemann
#
################################################################################
getwd() # aktueller Ordner

library(Hmisc)
library(tidyverse)

# Rohdaten liegen als SPSS-Daten vor
# label, level, missing values müssen sauber definiert sein!
daten <- spss.get("ESUdaten_DEMO.sav", allow = "_")

# alles als utf-8 speichern
encodingUTF8 <- function(x) {
  # zuerst die Variablennamen
  names(x) <- enc2utf8(names(x))
  # jetzt die Level und Label: für jede Variable einzeln
  for (i in 1:length(names(x))) {
    label(x[[i]]) <- enc2utf8(label(x[[i]]))
    if (is.factor(daten[[i]])) { 
      levels(x[[i]]) <- enc2utf8(levels(x[[i]])) 
    }
  }
  return(x)   
}
daten <- encodingUTF8(daten)

# die Levels pauschal trimmen
daten <- daten %>%
  mutate_if(is.factor, list(~`levels<-`(., trimws(levels(.)))))

# leere Werte auf NA setzen (betrifft die Strings)
daten <- daten %>%
  mutate_if(is.factor, na_if, "")

# die fehlenden Werte explizit (als factor-Level) setzen
daten <- daten %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "(fehlende Werte)")

# nicht vorhandene Levels löschen
daten <- daten %>% mutate_if(is.factor, ~fct_drop(., only = ""))

#### zum Schluss die Daten speichern
#save(daten, file = "ESUdaten.Rdata")
save(daten, file = "ESUdaten_DEMO.Rdata")

