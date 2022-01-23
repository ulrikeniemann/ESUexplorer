################################################################################
# ESU explorer
#
# functionsAnalysis.R
# Ulrike Niemann
#
################################################################################
# Konstanten 
# Grafikauswahl Varianten ------------------------------------------------------
# beide Vars mind. nominal:
choicesNominal <- c(
  "Säulen (gestapelt)",
  "Balken (gestapelt)",
  "Säulen (gruppiert)",
  "Balken (gruppiert)",
  "Linien",
  "Kreis"
)
# ZeilenVar metrisch, SpaltenVar kategorial:
choicesMetrisch <- c(
  "Boxplot/Violinplot",
  "Histogramm/Dichte"
)
# beide Vars metrisch:
choicesMetrisch2 <- c("Punkte")
#
################################################################################
#### Analysedaten ##############################################################
################################################################################
# Analysedaten kategoriale Variablen : Häufigkeiten ----------------------------
dataBase <- function(data, rowVar1, colVar1, rowVar2 = "") {
  rowVar1 <- sym(rowVar1)
  groupVars <- syms(c(colVar1, rowVar2))
  data <- data %>%
    group_by(!!!groupVars) %>%
    count(!!rowVar1) %>%
    mutate(
      prop = n / sum(n),
      ratio = scales::percent(n / sum(n), accuracy = 1)
    )
  return(data)
}
################################################################################
# Analysedaten für metrische ZeilenVar (als numeric abspeichern) ---------------
datNumVar <- function(data, rowVar1) {
  rowVar1 <- sym(rowVar1)
  data <- data %>%
    filter(!!rowVar1 != "(fehlende Werte)") %>%
    mutate(varNum = as.numeric(as.character(!!rowVar1)))
  label(data$varNum) <- daten %>%
    select(!!rowVar1) %>%
    label()
  return(data)
}
#
################################################################################
### Tabellen ###################################################################
################################################################################
# Häufigkeitstabelle absolut ---------------------------------------------------
tabFreq <- function(data) {
  tab <- data %>%
    select(-c("prop", "ratio")) %>%
    spread(1, n, fill = 0, convert = TRUE) %>%
    adorn_totals("row", na.rm = FALSE)
  colnames(tab)[1] <- ""
  return(tab)
}
################################################################################
# Häufigkeitstabelle absolut mit Unterteilung ----------------------------------
tabFreq2 <- function(data, rowVar1, rowVar2, colVar1) {
  tab <- data %>%
    select(-c("prop", "ratio")) %>%
    ungroup() %>%
    droplevels() %>%
    group_by(.dots = c(colVar1, rowVar2)) %>%
    do(plyr::rbind.fill(
      ., tibble(!!rowVar2 := first(eval(parse(text = paste0(".$", rowVar2)))),
        !!colVar1 := first(eval(parse(text = paste0(".$", colVar1)))),
        !!rowVar1 := c("Total"),
        n = c(sum(.$n, na.rm = T))
      )
    )) %>%
    mutate(!!rowVar1 :=
      factor(!!sym(rowVar1),
        levels = c(levels(daten[[rowVar1]]), "Total")
      )) %>%
    spread(1, n, fill = 0, convert = TRUE)
  return(tab)
}
################################################################################
# Häufigkeitstabelle relativ ---------------------------------------------------
tabPro <- function(data) {
  tab <- data %>%
    select(-c("n", "ratio")) %>%
    spread(1, prop, fill = 0, convert = TRUE) %>%
    adorn_totals("row", na.rm = FALSE)
  colnames(tab)[1] <- ""
  return(tab)
}
################################################################################
# Häufigkeitstabelle relativ mit Unterteilung ----------------------------------
tabPro2 <- function(data, rowVar1, rowVar2, colVar1) {
  tab <- data %>%
    select(-c("n", "ratio")) %>%
    ungroup() %>%
    droplevels() %>%
    group_by(.dots = c(colVar1, rowVar2)) %>%
    do(plyr::rbind.fill(
      ., tibble(!!rowVar2 := first(eval(parse(text = paste0(".$", rowVar2)))),
        !!colVar1 := first(eval(parse(text = paste0(".$", colVar1)))),
        !!rowVar1 := c("Total"),
        prop = c(sum(.$prop, na.rm = T))
      )
    )) %>%
    mutate(!!rowVar1 :=
      factor(!!sym(rowVar1),
        levels = c(levels(daten[[rowVar1]]), "Total")
      )) %>%
    spread(1, prop, fill = 0, convert = TRUE)
  return(tab)
}
################################################################################
# metrische Analysevariable: Lage/Streuungmaße  --------------------------------
# (in data ist hier varNum als nummerische Version der rowVar1 enthalten)
tabSum <- function(data, colVar1, rowVar2 = "", metrisch2 = FALSE) {
  # keine kreuztabellarische Anforderung bei 2 metrischen Vars
  # 2 metrische Vars wird die große Ausnahme bleiben
  if (metrisch2 == TRUE) {
    colVar1 <- "Gesamt"
  }
  groupVars <- syms(c(colVar1, rowVar2))
  tab <- data %>%
    group_by(!!!groupVars) %>%
    summarise(
      Minimum = min(varNum),
      "1. Quartil" = quantile(varNum, 0.25),
      Median = median(varNum),
      "3. Quartil" = quantile(varNum, 0.75),
      Maximum = max(varNum),
      Mittelwert = mean(varNum),
      "Standard-Abweichung" = sd(varNum) * sqrt((n() - 1) / n()),
      Fallzahl = n()
    )
  # einfache Analyse:
  if (rowVar2 == "") {
    tabTransp <- tab %>%
      select(-1) %>%
      t() %>%
      as.data.frame() %>%
      set_names(tab[1][[1]]) %>%
      round(digits = 2) %>%
      rownames_to_column(var = "sum")
    colnames(tabTransp)[1] <- "Maßzahl"
  } else {
    # unterteilte Analyse:
    tabTransp <- tab %>%
      gather(key = "key", value = "value", 3:10) %>%
      mutate(value = round(value, digits = 2)) %>%
      mutate(
        key =
          factor(key, levels = c(
            "Minimum", "1. Quartil", "Median",
            "3. Quartil", "Maximum", "Mittelwert",
            "Standard-Abweichung", "Fallzahl"
          ))
      ) %>% 
      spread(colVar1, value)
    colnames(tabTransp)[2] <- "Maßzahl"
  }
  return(tabTransp)
}
################################################################################
### Grafiken ###################################################################
################################################################################
# allgmeines Thema für die normalen Charts -------------------------------------
theme_chart <- function(base_size = 14, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major.y = element_line(colour = "grey85"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "grey85", colour = NA)
    )
}
################################################################################
# Farbpalette basierend auf der Anzahl Levels ----------------------------------
# es wird immer für alle Levels berechnet damit die Farben gleich bleiben
colBars <- function(dat, color) {
  levels <- levels(dat[[ncol(dat) - 3]]) 
  # nur bei vielen Levels noch mal schauen, ob doch weniger in den Daten sind
  if (length(levels) > 25) {
    levels <- levels(droplevels(dat[[ncol(dat) - 3]]))
  }
  nLevels <- length(levels)
  nLevels <- nLevels + 1 # die erste Farbe ist zu hell
  # mehr als 25 Levels nicht mehr darstellen
  if (nLevels > 25) {
    return(NULL)
  }
  # wenn fehlende Werte dann eine Farbe abziehen
  if (any(levels == "(fehlende Werte)")) {
    nLevels <- nLevels - 1
  }
  # wie sollen die Farben aufgebaut werden?
  if (color == "Oranges") {
    cols <- c("Oranges", "Blues", "Greens")
  } else if (color == "Blues") {
    cols <- c("Blues", "Oranges", "Greens")
  } else if (color == "Greens") {
    cols <- c("Greens", "Oranges", "Blues")
  }
  if (nLevels <= 9) {
    colBars <- brewer.pal(max(3, nLevels), cols[1])
  } else if (nLevels <= 17) { # bis 18 Farben
    colBars <- c(
      brewer.pal(9, cols[1]),
      brewer.pal(max(3, nLevels - 8), cols[2])[-1]
    )
  } else if (nLevels <= 25) { # 18 - 25 Farben: mehr dürfte nicht nötig sein
    colBars <- c(
      brewer.pal(9, cols[1]),
      brewer.pal(9, cols[2])[-1],
      brewer.pal(max(3, nLevels - 16), cols[3])[-1]
    )
  }
  # maximale Länge
  colBars <- colBars[1:nLevels]
  # wenn fehlende Werte dann grau darstellen:
  if (any(levels == "(fehlende Werte)")) {
    colBars <- c(colBars, "#BDBDBD")
  }
  # Rückgabe: Levels + entsprechende Farben
  levelsColor <- cbind.data.frame(levels,
    colorPlot = colBars[-1],
    stringsAsFactors = FALSE
  )
  # einen named vector erzeugen für die richtige Farbzuordnung
  levelsColor <- levelsColor %>% deframe()
  return(levelsColor)
}
#
################################################################################
# Diagramm Kategorial ##########################################################
################################################################################
plotCat <- function(data, col,
                    rowVar1,
                    colVar1,
                    rowVar2 = "",
                    plotType,
                    legende = "Right",
                    trend = FALSE,
                    baseSize = 14,
                    geomTextSize = 14) {
  baseSize <- as.numeric(baseSize)
  geomTextSize <- (as.numeric(geomTextSize)) / 3.5 # 3.608247
  # Labels der Analysevariablen ------------------------------------------------
  labelRowVar <- str_wrap(varLabels[rowVar1], width = 30)
  labelColVar <- varLabels[colVar1]
  # tidy evaluation ------------------------------------------------------------
  rowVar1 <- sym(rowVar1)
  colVar1 <- sym(colVar1)
  if (rowVar2 != "") rowVar2 <- sym(rowVar2)
  # Plot aufbauen --------------------------------------------------------------
  # für alle außer Kreis:
  if (plotType != "Kreis") {
    plot <- ggplot(data, aes(x = !!colVar1, y = prop, group = !!rowVar1)) +
      scale_x_discrete(
        name = labelColVar,
        labels = function(x) str_wrap(x, width = 20)
      ) +
      scale_y_continuous(
        name = labelRowVar,
        breaks = seq(0, 1, 0.2),
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1.05)
      ) +
      scale_fill_manual(
        name = labelRowVar,
        values = col
      ) +
      theme_chart(base_size = baseSize) +
      theme(legend.position = legende)
    if (rowVar2 != "") {
      plot <- plot +
        facet_wrap(vars(!!rowVar2),
          nrow = 1,
          labeller = labeller(.rows = label_wrap_gen(20))
        )
    }
    # gestaplete Säulen + Balken -----------------------------------------------
    if (plotType == "Säulen (gestapelt)" || plotType == "Balken (gestapelt)") {
      plot <- plot +
        geom_col(aes(fill = !!rowVar1),
          position = position_stack(reverse = TRUE)
        ) +
        geom_text(aes(label = ratio),
          size = geomTextSize,
          position = position_stack(reverse = TRUE, vjust = .5)
        )
      # Balken statt Säulen?
      if (plotType == "Balken (gestapelt)") {
        plot <- plot +
          coord_flip() +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "grey80")
          )
      }
    }
    # gruppierte Säulen --------------------------------------------------------
    if (plotType == "Säulen (gruppiert)") {
      plot <- plot +
        geom_col(aes(fill = !!rowVar1), position = "dodge") +
        geom_text(aes(label = ratio),
          size = geomTextSize,
          position = position_dodge(width = 0.9),
          vjust = -0.5
        )
    }
    # gruppierte Balken --------------------------------------------------------
    if (plotType == "Balken (gruppiert)") {
      plot <- plot +
        geom_col(aes(fill = !!rowVar1),
          position = position_dodge2(reverse = TRUE)
        ) +
        geom_text(aes(label = ratio),
          size = geomTextSize,
          position = position_dodge2(reverse = TRUE, width = 0.9),
          hjust = -0.1
        ) +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = "grey80")
        )
    }
    # Linien -------------------------------------------------------------------
    if (plotType == "Linien") {
      plot <- plot +
        geom_line(aes(colour = !!rowVar1, group = !!rowVar1),
          stat = "identity",
          size = 1.5
        ) +
        geom_point(aes(colour = !!rowVar1),
          size = 3
        ) +
        geom_text(aes(label = ratio), size = geomTextSize, vjust = -1) +
        scale_colour_manual(
          name = labelRowVar,
          values = col
        )

      # Linearen Trend anzeigen?
      if (trend == TRUE) {
        plot <- plot + geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = FALSE,
          size = 1,
          aes(fill = "linearer Trend")
        )
      }
    }
  } else {
    # Kreisdia (anders aufgebaut) ----------------------------------------------
    plot <- ggplot(data, aes(x = "", y = prop, fill = !!rowVar1)) +
      geom_bar(
        stat = "identity",
        position = position_stack(reverse = TRUE), width = 1
      ) +
      coord_polar("y", start = 0) +
      scale_x_discrete(name = labelRowVar) +
      scale_y_continuous(
        name = labelColVar,
        breaks = NULL,
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      scale_fill_manual(
        name = labelRowVar,
        values = col
      ) +
      geom_text(aes(label = ratio),
        size = geomTextSize,
        position = position_stack(reverse = TRUE, vjust = 0.5)
      ) +
      theme_chart(base_size = baseSize) +
      theme(legend.position = legende) +
      theme(
        strip.placement = "outside",
        strip.background.y = element_rect(fill = "grey85", colour = NA),
        strip.background.x = element_rect(fill = NA, colour = NA)
      )
    # mit Unterteilung?
    if (rowVar2 != "") {
      plot <- plot +
        facet_grid(
          rows = vars(!!rowVar2),
          cols = vars(!!colVar1),
          switch = "x",
          labeller = labeller(.rows = label_wrap_gen(20))
        )
    } else {
      plot <- plot +
        facet_grid(
          cols = vars(!!colVar1), switch = "x",
          labeller = labeller(.cols = label_wrap_gen(20))
        )
    }
  }
  # Legende in einer Spalte ----------------------------------------------------
  # alle außer Linien:
  if (plotType != "Linien") {
    # gestapelte Säulen? (dann reverse sonst nicht)
    if (plotType == "Säulen (gestapelt)") {
      plot <- plot + guides(fill = guide_legend(
        nrow = length(col),
        reverse = TRUE
      ))
    } else {
      plot <- plot + guides(fill = guide_legend(nrow = length(col)))
    }
  } else { # Linien
    plot <- plot + guides(colour = guide_legend(nrow = length(col)))
  }
  # ----------------------------------------------------------------------------
  return(plot)
}
################################################################################
# Diagramm metrisch ############################################################
################################################################################
plotNum <- function(data,
                    rowVar1, colVar1,
                    rowVar2 = "",
                    rowHide2, colHide,
                    plotType,
                    boxplot,
                    violin,
                    mw,
                    bins = 20,
                    density,
                    baseSize = 14,
                    trend) {
  baseSize <- as.numeric(baseSize)
  # Labels der Analysevariablen
  labelRowVar <- str_wrap(varLabels[rowVar1], width = 30)
  labelColVar <- varLabels[colVar1]
  # tidy evaluation
  rowVar1 <- sym(rowVar1)
  colVar1 <- sym(colVar1)
  if (rowVar2 != "") {
    labelRowVar2 <- str_wrap(varLabels[rowVar2], width = 30)
    rowVar2 <- sym(rowVar2)
  }
  # Kategorien ausblenden?
  if (length(rowHide2) > 0) {
    data <- data %>% filter(!(!!rowVar2) %in% rowHide2)
  }
  if (length(colHide) > 0) {
    data <- data %>% filter(!(!!colVar1) %in% colHide)
  }
  if (any(colnames(data) == "Gesamt")) {
    levels(data$Gesamt) <- ""
  }
  # für Boxplots ---------------------------------------------------------------
  if (plotType == "Boxplot/Violinplot") {
    plot <- ggplot(data, aes(x = !!colVar1, y = varNum)) +
      scale_x_discrete(
        name = labelColVar,
        labels = function(x) str_wrap(x, width = 20)
      ) +
      scale_y_continuous(name = labelRowVar) +
      theme_chart(base_size = baseSize)
    # Violinplot?
    if (violin == TRUE) {
      plot <- plot + geom_violin(fill = "lightblue")
    }
    # Boxplot?
    if (boxplot == TRUE) {
      plot <- plot +
        stat_boxplot(geom = "errorbar", width = 0.05) +
        geom_boxplot(fill = "royalblue1", width = 0.2)
    }
    # Mittelwert?
    if (mw == TRUE) {
      plot <- plot +
        stat_summary(
          fun.y = mean, geom = "point",
          shape = 20, size = 5, color = "navyblue"
        )
    }
    # Unterteilung?
    if (rowVar2 != "") {
      plot <- plot +
        facet_wrap(vars(!!rowVar2),
          nrow = 1,
          labeller = labeller(.rows = label_wrap_gen(20))
        )
    }
  }
  # Histogramm/Dichte wird anders aufgebaut ------------------------------------
  bins <- as.numeric(bins)
  if (plotType == "Histogramm/Dichte") {
    plot <- ggplot(data, aes(x = varNum)) +
      geom_histogram(aes(y = ..density..),
        bins = bins, fill = "royalblue1"
      ) +
      theme_chart(base_size = baseSize) +
      theme(
        strip.placement = "outside",
        strip.background.y = element_rect(fill = "grey85", colour = NA),
        strip.background.x = element_rect(fill = NA, colour = NA)
      ) +
      scale_y_continuous(
        name = str_wrap(paste0(labelRowVar, " (Dichte)"),
          width = 30
        ),
        breaks = NULL
      ) +
      scale_x_continuous(name = labelColVar)
    # Dichteschätzung?
    if (density == TRUE) {
      plot <- plot +
        geom_density(fill = "blue", color = "black", alpha = 0.5)
    }
    # Unterteilung?
    if (rowVar2 != "") {
      plot <- plot +
        facet_grid(
          rows = vars(!!rowVar2),
          cols = vars(!!colVar1),
          switch = "x",
          labeller = labeller(.rows = label_wrap_gen(20))
        )
    } else {
      plot <- plot +
        facet_grid(
          cols = vars(!!colVar1), switch = "x",
          labeller = labeller(.cols = label_wrap_gen(20))
        )
    }
  }
  # für Punkte -----------------------------------------------------------------
  if (plotType == "Punkte") {
    # quadratischer Output
    maxY <- data %>%
      select(varNum) %>%
      pull() %>%
      max()
    minY <- data %>%
      select(varNum) %>%
      pull() %>%
      min()
    maxX <- data %>%
      select(!!colVar1) %>%
      pull() %>%
      as.character() %>%
      as.numeric() %>%
      max()
    minX <- data %>%
      select(!!colVar1) %>%
      pull() %>%
      as.character() %>%
      as.numeric() %>%
      min()
    ratio.values <- (maxX - minX) / (maxY - minY)
    #
    plot <- ggplot(data, aes(
      x = as.numeric(as.character(!!colVar1)),
      y = varNum
    )) +
      geom_point(colour = "royalblue1", alpha = 1 / 5) +
      coord_fixed(ratio = ratio.values / 1) +
      xlab(labelColVar) +
      scale_y_continuous(name = labelRowVar) +
      theme_chart(base_size = baseSize)
    # Unterteilung?
    if (rowVar2 != "") {
      plot <- plot +
        facet_wrap(vars(!!rowVar2),
          nrow = 1,
          labeller = labeller(.rows = label_wrap_gen(20))
        )
    }
    # Linearen Trend anzeigen?
    if (trend == TRUE) {
      plot <- plot + geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        size = 1
      )
    }
    # Boxplots?
    if (boxplot == TRUE) {
      if (rowVar2 == "") { # geht nur ohne Unterteilung!
        plot <- ggMarginal(plot,
          type = "boxplot",
          fill = "royalblue1", width = 0.1
        )
      }
    }
    # Dichte?
    if (density == TRUE) {
      if (rowVar2 == "") { # geht nur ohne Unterteilung!
        plot <- ggMarginal(plot,
          type = "density",
          fill = "royalblue1"
        )
      }
    }
  }
  # ----------------------------------------------------------------------------
  return(plot)
}
################################################################################
#### Maßzahlen Zusammenhänge ###################################################
################################################################################
# Interpretationshilfe (nach Cohen 1988)
interpretation <- function(Wert) {
  case_when(
    Wert < 0.1 ~ "Wert < 0,1: kein Zusammenhang",
    Wert < 0.3 ~ "0,1 > Wert < 0,3: schwacher Zusammenhang",
    Wert < 0.5 ~ "0,3 > Wert < 0,5: mittlerer Zusammenhang",
    Wert < 1 ~ "0,5 > Wert < 1: starker Zusammenhang",
    Wert == 1 ~ "Wert = 1: perfekter Zusammenhang"
  )
}
################################################################################
# Erstellen der Tabelle --------------------------------------------------------
coeff <- function(data, rowVar1, colVar1, rowVar2 = "") {
  # tidy evaluation
  rowVar1 <- sym(rowVar1)
  colVar1 <- sym(colVar1)
  if (rowVar2 != "") {
    groupVars <- syms(c(colVar1, rowVar2))
    rowVar2 <- sym(rowVar2)
    data <- data %>% group_by(!!rowVar2)
    # --------------------------------------------------------------------------
    # Fehlerbehandlung: nur bei Unterteilung notwendig
    chiTest <- data %>%
      droplevels() %>%
      summarise(
        "Chi-Quadrat" =
          tryCatch(chisq.test(!!colVar1, !!rowVar1)$statistic,
            error = function(e) {
              NA
            }
          )
      )
    noTestLevel <- chiTest[is.na(chiTest[, 2]), 1]
    # wenn bei einzelnen Levels kein Test möglich, dann rausfiltern
    # weiter unten werden entsprechende Hinweise erzeugt.
    data <- data %>%
      filter(!(!!rowVar2) %in% pull(noTestLevel))
    # wenn jetzt keine Zeilen mehr vorhanden sind, leere Tabelle anlegen
    if (nrow(data) == 0) {
      as.data.frame(cbind("Maßzahl" = NA, "Wert" = NA, "Interpretation" = NA))
    }
  }
  # ----------------------------------------------------------------------------
  # um zu entscheiden ob weitere Maßzahlen berechnet werden sollen:
  # ordinal: eine muss ordinal sein, die andere ordinal oder metrisch
  ordinal <- ((meta[meta$Variable == rowVar1, "Meßart"] == "ordinal" &&
    meta[meta$Variable == colVar1, "Meßart"] != "nominal") ||
    (meta[meta$Variable == rowVar1, "Meßart"] != "nominal" &&
      meta[meta$Variable == colVar1, "Meßart"] == "ordinal"))
  # metrisch: beide müssen metrisch sein
  metrisch <- (meta[meta$Variable == rowVar1, "Meßart"] == "metrisch" &&
    meta[meta$Variable == colVar1, "Meßart"] == "metrisch")
  # ----------------------------------------------------------------------------
  coeff <- data %>%
    summarise(
      "Chi-Quadrat" = chisq.test(!!colVar1, !!rowVar1)$statistic,
      "Freiheitsgrade" = chisq.test(!!colVar1, !!rowVar1)$parameter,
      "p-Wert Chi-Quadrat" = chisq.test(!!colVar1, !!rowVar1)$p.value,
      "Cramers V" =
        CramerV(droplevels(!!colVar1), droplevels(!!rowVar1)),
      "Pearsons Kontingenzkoeffizient" =
        ContCoef(droplevels(!!colVar1),
          droplevels(!!rowVar1),
          correct = FALSE
        ),
      "Pearsons Kontingenzkoeffizient (korrigiert)" =
        ContCoef(droplevels(!!colVar1),
          droplevels(!!rowVar1),
          correct = TRUE
        )
    )
  # mindestens ordinale Vars ---------------------------------------------------
  if (ordinal == TRUE) {
    # zuerst die Vars in nummerisch umwandeln
    # Zeilenvariable
    if (meta[meta$Variable == rowVar1, "Meßart"] == "metrisch") {
      data <- data %>%
        mutate(rowNum = as.numeric(as.character(!!rowVar1)))
    } else { # oridinal
      data <- data %>%
        mutate(rowNum = as.numeric(!!rowVar1)) %>%
        mutate(rowNum = ifelse(!!rowVar1 == "(fehlende Werte)", NA, rowNum))
    }
    # Spaltenvariable
    if (meta[meta$Variable == colVar1, "Meßart"] == "metrisch") {
      data <- data %>%
        mutate(colNum = as.numeric(as.character(!!colVar1)))
    } else { # oridinal
      data <- data %>%
        mutate(colNum = as.numeric(!!colVar1)) %>%
        mutate(colNum = ifelse(!!colVar1 == "(fehlende Werte)", NA, colNum))
    }
    # Koeffizienten berechnen
    coeff <- add_column(coeff, "Spearmans Rangkorrelation" = data %>%
      summarise(
        "Spearman" =
          cor.test(colNum, rowNum,
            method = "spearman",
            exact = FALSE
          )$estimate
      ) %>%
      select("Spearman") %>% pull())
    coeff <- add_column(coeff, "p-Wert Spearmans Rangkorrelation" = data %>%
      summarise(
        "Spearman p" =
          cor.test(colNum, rowNum,
            method = "spearman",
            exact = FALSE
          )$p.value
      ) %>%
      select("Spearman p") %>% pull())
    # # Kendalls Tau braucht ewig, zu Rechnerintensiv - weglassen!
    # coeff <- add_column(coeff, "Kendalls Rangkorrelation" = data %>%
    #                       summarise(
    #                         "Kendall" = cor.test(colNum, rowNum,
    #                                              method = 'kendall',
    #                                              exact=FALSE)$estimate) %>%
    #                       select("Kendall") %>% pull())
    # coeff <- add_column(coeff, "p-Wert Kendalls Rangkorrelation" = data %>%
    #                       summarise(
    #                         "Kendall p" = cor.test(colNum, rowNum,
    #                                                method = 'kendall',
    #                                                exact=FALSE)$p.value) %>%
    #                       select("Kendall p") %>% pull())
  }
  # mindestens metrische Vars --------------------------------------------------
  if (metrisch == TRUE) {
    # Spearman für monotone UND Pearson für lineare Zushg:
    coeff <- add_column(coeff, "Spearmans Rangkorrelation" = data %>%
      summarise(
        "Spearman" =
          cor.test(as.numeric(as.character(!!colVar1)),
            as.numeric(as.character(!!rowVar1)),
            method = "spearman"
          )$estimate
      ) %>%
      select("Spearman") %>% pull())
    coeff <- add_column(coeff, "p-Wert Spearmans Rangkorrelation" = data %>%
      summarise(
        "Spearman p" =
          cor.test(as.numeric(as.character(!!colVar1)),
            as.numeric(as.character(!!rowVar1)),
            method = "spearman"
          )$p.value
      ) %>%
      select("Spearman p") %>% pull())
    coeff <- add_column(coeff, "Pearsons Korrelationskoeffizient" = data %>%
      summarise(
        "Pearson" =
          cor.test(as.numeric(as.character(!!colVar1)),
            as.numeric(as.character(!!rowVar1)),
            method = "pearson"
          )$estimate
      ) %>%
      select("Pearson") %>% pull())
    coeff <- add_column(coeff,
      "p-Wert Pearsons Korrelationskoeffizient" =
        data %>%
          summarise(
            "Pearson p" =
              cor.test(as.numeric(as.character(!!colVar1)),
                as.numeric(as.character(!!rowVar1)),
                method = "pearson"
              )$p.value
          ) %>%
          select("Pearson p") %>% pull()
    )
  }
  # Tabelle aufbereiten --------------------------------------------------------
  coeff <- coeff %>%
    tbl_df()
  if (rowVar2 == "") {
    coeff <- coeff %>%
      mutate_at(vars(-"Freiheitsgrade", ), ~ sprintf("%.3f", .)) %>%
      gather(key = "Maßzahl", value = "Wert")
  } else {
    coeff <- coeff %>%
      mutate_at(vars(-c("Freiheitsgrade", !!rowVar2)), ~ sprintf("%.3f", .)) %>%
      gather(key = "Maßzahl", value = "Wert", -!!rowVar2)
  }
  ### noch eine Spalte mit den entsprechenden Interpretationen -----------------
  coeff <- coeff %>% mutate("Interpretation" = "")
  # Chi-Quadrat
  # Warnmeldung: gibt es erwartete Werte < 5?
  warn <- data %>%
    summarise(
      "Warnung" =
        any(chisq.test(!!colVar1, !!rowVar1)$expected < 5)
    ) %>%
    pull()
  coeff[coeff$Maßzahl == "Chi-Quadrat", ] <- coeff %>%
    filter(Maßzahl == "Chi-Quadrat") %>%
    mutate(
      "Interpretation" =
        ifelse(warn == TRUE,
          "Warnung: Chi-Quadrat-Test unzuverlässig, da erwartete Häufigkeiten < 5 enthalten.",
          ""
        )
    )
  # p-wert
  coeff[str_sub(coeff$Maßzahl, 1, 6) == "p-Wert", ] <- coeff %>%
    filter(str_sub(Maßzahl, 1, 6) == "p-Wert") %>%
    mutate(
      "Interpretation" =
        ifelse(as.numeric(Wert) < 0.05,
          "p-Wert < 0,05: signifikanter Zusammenhang",
          "p-Wert > 0,05: kein signifikanter Zusammenhang"
        )
    )
  # hier für alle Korrelationsmaßzahlen:
  coeff[coeff$Maßzahl %in% c(
    "Cramers V",
    "Pearsons Kontingenzkoeffizient",
    "Pearsons Kontingenzkoeffizient (korrigiert)",
    "Spearmans Rangkorrelation",
    # "Kendalls Rangkorrelation",
    "Pearsons Korrelationskoeffizient"
  ), ] <-
    coeff[coeff$Maßzahl %in% c(
      "Cramers V",
      "Pearsons Kontingenzkoeffizient",
      "Pearsons Kontingenzkoeffizient (korrigiert)",
      "Spearmans Rangkorrelation",
      # "Kendalls Rangkorrelation",
      "Pearsons Korrelationskoeffizient"
    ), ] %>%
    mutate("Interpretation" = as.numeric(Wert) %>% interpretation())
  # Levels anfügen, für die kein Test berechnet wurde --------------------------
  if (rowVar2 != "") {
    coeff <- full_join(noTestLevel, coeff, by = as_name(rowVar2))
    # bei Interpretation noch ein Hinweis auf fehlende Berechnung:
    coeff <- coeff %>%
      mutate(
        "Interpretation" =
          ifelse(is.na(coeff$Maßzahl),
            "Keine Maßzahlen wegen unzureichender Dimensionalität der Tabelle.",
            coeff$Interpretation
          )
      )
    # --------------------------------------------------------------------------
    # bei Unterteilung die Maßzahlen danach anordnen
    coeff <- coeff %>% arrange(!!rowVar2)
  }
  # ----------------------------------------------------------------------------
  return(coeff)
}
################################################################################
