################################################################################
# ESU explorer
#
# functionsMaps.R
# Ulrike Niemann
#
################################################################################
# Konstanten
# bis zu welcher Fallzahl sollen die Ergebnisse nicht/grau dargestellt werden?
smallN <- 10
# welches Quantil soll bei den Hotspot-Karten dargestellt werden?
hotspotQuantile <- 0.9
################################################################################
# (1) myDataSet$data <- dataFilter (functions) ---------------------------------
################################################################################
# (2) Tabellenbasis für Tabellenoutput -----------------------------------------
createTabData <- function(data, var, raum, jahr) {
  # tidy evaluation ------------------------------------------------------------
  vars <- syms(c(var, raum))
  groupVars <- syms(c(zeitVar, raum))
  var <- sym(var)
  # die ESB Levels nur wenn im Jahr vorhanden ----------------------------------
  if (raum == "ESB") {
    esbLevels <- daten %>%
      filter(!!sym(zeitVar) == jahr) %>%
      select(ESB) %>%
      deframe() %>%
      droplevels() %>%
      levels()
    data <- data %>%
      mutate(ESB = factor(ESB, levels = esbLevels))
  }
  # Tabellarischer Output ------------------------------------------------------
  tab <- data %>%
    droplevels(except = raum) %>%
    group_by(!!!groupVars) %>%
    count(!!!vars, .drop = FALSE) %>%
    mutate(
      total = sum(n),
      prop = n / sum(n)
    ) %>%
    # filter(!!sym(var) == level) %>% # nur das entsprechende Level
    filter(!!sym(raum) != "(fehlende Werte)") %>%
    mutate(prop = ifelse(is.na(prop), NA, prop)) %>%
    mutate(propTip = ifelse(is.na(prop),
      "-", sprintf(fmt = "%1.1f%%", prop * 100)
    ))
  # ----------------------------------------------------------------------------
  return(tab)
}
################################################################################
# (3) Daten für ggplot-map aufbauen --------------------------------------------
createMapData <- function(tab, jahr, level) {
  tab <- tab %>%
    # nur die darzustellenden Level:
    filter(!!sym(colnames(tab)[3]) == level) %>%
    # Fallzahl < smallN: nicht auf der Karte darstellen
    mutate(prop = ifelse(total < smallN, NA, prop))
  # shp-file festlegen ---------------------------------------------------------
  shp <- colnames(tab)[2]
  if (shp == "PLR") {
    mapDf <- "berlinMittePLRPoints"
  } else if (shp == "BZR") {
    mapDf <- "berlinMitteBZRPoints"
  } else if (shp == "PRG") {
    mapDf <- "berlinMittePGRPoints"
  } else if (shp == "BEZ") {
    mapDf <- "berlinMitteBEZPoints"
  } else if (shp == "ESB") {
    if (jahr == "2019") {
      mapDf <- "berlinMitteESB2019Points"
    }
    if (jahr == "2018") {
      mapDf <- "berlinMitteESB2018Points"
    }
    if (jahr == "2016" || jahr == "2015") {
      mapDf <- "berlinMitteESB2015Points"
    }
    if (jahr == "2014" || jahr == "2013" || jahr == "2012") {
      mapDf <- "berlinMitteESB2012Points"
    }
    if (jahr == "2010" || jahr == "2011") {
      mapDf <- "berlinMitteESB2011Points"
    }
  }
  mapDf <- sym(mapDf)
  # mapData aufbauen: entweder LOR oder ESB ------------------------------------
  if (!str_detect(shp, "ESB")) { # hier für LOR --------------------------------
    # id anspielen (raum als chraracter), title: Variable+Level
    tab <- tab %>%
      mutate(
        id = str_trim(as.character(!!sym(colnames(tab)[2]))),
        title = paste0(
          varLabels[as_name(colnames(tab)[3])],
          " - ", tab[, 3] %>% distinct() %>% pull()
        )
      )
    # mit den Polygon-Daten matchen
    mapData <- left_join(tab, eval(mapDf), by = "id")
    # tooltip: für die LOR mit id und Name
    mapData <- mapData %>% mutate("tip" = paste0(
      "Schuljahr: ", !!sym(zeitVar),
      "<br><b>", id, " - ", Name,
      "</b>:",
      "<br>Anteil: <i>", title,
      "</i><br>", propTip,
      "<br>(n = ", n,
      ", von Total = ", total, ")"
    ))
  } else { # hier für ESB ------------------------------------------------------
    # title: Variable+Level
    tab <- tab %>%
      mutate(title = paste0(
        varLabels[as_name(colnames(tab)[3])],
        " - ", tab[, 3] %>% distinct() %>% pull()
      ))
    # bei ESB gibt es fehlende Werte - rausnehmen
    tab <- tab[tab[, 2] != "(fehlende Werte)", ]
    # mit den Polygon-Daten matchen
    mapData <- left_join(tab %>% droplevels(),
      eval(mapDf),
      by = "ESB"
    )
    # tooltip: ESB ohne id/Name
    mapData <- mapData %>% mutate("tip" = paste0(
      "Schuljahr: ", !!sym(zeitVar),
      "<br><b>ESB ", ESB,
      "</b>:",
      "<br>Anteil: <i>", title,
      "</i><br>", propTip,
      "<br>(n = ", n,
      ", von Total = ", total, ")"
    ))
  }
}
################################################################################
# (4) Daten für die Kerndichtekarte berechnen ----------------------------------
createKernelheapingPlotData <- function(mapData,
                                        jahr,
                                        burnin = 2,
                                        samples = 5,
                                        gridsize = 300) {
  # welches shp-file nutzen? shp muss mit in die kernelheaping-Routine ---------
  shp <- colnames(mapData[2])
  if (shp == "PLR") {
    mapDf <- "berlinMitte"
  } else if (shp == "BZR") {
    mapDf <- "berlinMitteBZR"
  } else if (shp == "PRG") {
    mapDf <- "berlinMittePGR"
  } else if (shp == "BEZ") {
    mapDf <- "berlinMitteBEZ"
  } else if (shp == "ESB") {
    if (jahr == "2019") {
      mapDf <- "esb2019"
    }
    if (jahr == "2018") {
      mapDf <- "esb2018"
    }
    if (jahr == "2016" || jahr == "2015") {
      mapDf <- "esb2015"
    }
    if (jahr == "2014" || jahr == "2013" || jahr == "2012") {
      mapDf <- "esb2012"
    }
    if (jahr == "2010" || jahr == "2011") {
      mapDf <- "esb2011"
    }
    # 2010 ? fehlendes shp-File
  }
  # dataIn aufbereiten ---------------------------------------------------------
  mapDf <- sym(mapDf)
  # erst für die LOR
  if (shp %in% c("PLR", "BZR", "PGR", "BEZ")) {
    dataIn <- mapData %>%
      ungroup() %>%
      select(longCenter, latCenter, n, total) %>%
      distinct() %>%
      as.data.frame()
  } else { # ESB: das muss nach der id aus dem shp-file sortiert sein!
    dataIn <- mapData %>%
      ungroup() %>%
      mutate(id = as.numeric(id)) %>%
      arrange(id) %>%
      select(longCenter, latCenter, n, total) %>%
      distinct() %>%
      as.data.frame()
  }
  # Daten berechnen ------------------------------------------------------------
  est <- custom_dshapebivrProp(
    data = dataIn,
    burnin = burnin,
    samples = samples,
    shapefile = eval(mapDf),
    gridsize = gridsize,
    boundary = FALSE, # zu rechenintensiv!
    deleteShapes = NULL, # deutlich zu unruhig, soll nicht angeboten werden
    numChains = 3
  )
  # Daten aufbereiten ----------------------------------------------------------
  kData <- data.frame(expand.grid(
    long = est$Mestimates$eval.points[[1]],
    lat = est$Mestimates$eval.points[[2]]
  ),
  Proportion = est$proportions %>% as.vector()
  ) %>%
    filter(Proportion > 0)
}
################################################################################
#### ggplots aufbauen ##########################################################
################################################################################
# Layer die in beiden Kartenvarianten gleich bleiben ---------------------------
waterLayer <- geom_polygon(
  data = berlinMitteWater,
  fill = "deepskyblue3",
  aes(long, lat, group = group)
)
greenLayer <- geom_polygon(
  data = berlinMitteGreen,
  fill = "darkolivegreen4",
  aes(long, lat, group = group)
)
otherLayer <- geom_polygon(
  data = berlinMitteOther,
  fill = "grey50",
  aes(long, lat, group = group)
)
schulenLayer <- list(
  geom_point_interactive(
    data = map_schulen,
    aes(long, lat,
      size = "Schulen",
      tooltip = Schulname,
      data_id = Schulname
    ),
    shape = 21, fill = "#8470FF"
  ),
  guides(size = guide_legend(title = NULL))
)
pathLayer <- geom_path(aes(long, lat, group = group),
  color = "gray30",
  size = 0.5
)
pathLayerMitte <- geom_path(
  data = berlinMitteBEZ, color = "gray30",
  aes(long, lat, group = group), size = 0.5
)
labGrau <- str_c("Fallzahl < ", smallN)
#
################################################################################
# normale Choroplethenkarte ----------------------------------------------------
createGgplot <- function(mapData,
                         valueShow = TRUE,
                         percentHide = FALSE,
                         geomTextSize = 14,
                         scale = "pretty",
                         scaleNumber,
                         quantileNumber,
                         baseSize = 14,
                         legende = "right",
                         color = "OrRd",
                         path = TRUE,
                         schulen = FALSE,
                         water = FALSE,
                         green = FALSE,
                         other = FALSE) {
  baseSize <- as.numeric(baseSize)
  geomTextSize <- (as.numeric(geomTextSize)) / 3.5 # 3.608247
  # ----------------------------------------------------------------------------
  # Diskrete Skala -------------------------------------------------------------
  if (scale != "continuous") {
    if (scale == "pretty") {
      mapData$varCut <- cut(mapData$prop * 100,
        breaks = pretty(mapData$prop * 100),
        include.lowest = TRUE
      )
    } else if (scale == "interval") {
      mapData$varCut <- cut_width(mapData$prop * 100,
        width = scaleNumber,
        boundary = 0
      )
    } else if (scale == "quantile") {
      mapData$varCut <- cut_interval(mapData$prop * 100,
        n = quantileNumber
      )
    }
    # Labels für die Skala/Legende aufbauen ------------------------------------
    lab <- mapData$varCut %>%
      droplevels() %>%
      levels()
    # Label für unbewohnt / zu wenig gültige Fälle
    if (any(is.na(mapData$varCut))) {
      lab <- c(lab, labGrau)
    }
    # in den Labels Angaben mit %
    labNum <- lab[lab != labGrau]
    labNum <- labNum %>%
      str_replace("\\[", "") %>%
      str_replace("\\(", "> ") %>%
      str_replace(",", "% - ") %>%
      str_replace("\\]", "%")
    lab[lab != labGrau] <- labNum
    # je nach dem wieviele Farben wird unten die Farbskala erzeugt -------------
    noOfColors <- length(lab)
    if (any(is.na(mapData$prop))) {
      noOfColors <- noOfColors - 1
    }
    # ggplot aufbauen ----------------------------------------------------------
    map <- ggplot(data = mapData) +
      geom_polygon_interactive(aes(long, lat,
        group = group,
        fill = varCut,
        tooltip = tip, data_id = id
      )) +
      coord_map() +
      theme_void(base_size = baseSize) +
      theme(legend.position = legende)
    if (noOfColors < 10) {
      # cols <- brewer.pal(noOfColors + 1, color)[-1] # falls erste Farbe zu hell
      cols <- suppressWarnings(brewer.pal(noOfColors, color))
    } else { # mehr als 9 Farben
      cols <- colorRampPalette(brewer.pal(9, color))(noOfColors + 1)[-1]
    }
    map <- map +
      scale_fill_manual(
        values = cols,
        name = NULL,
        na.value = "grey60",
        labels = lab
      )
  # ----------------------------------------------------------------------------
  # kontinuierliche Skala: ggplot aufbauen -------------------------------------
  } else {
    map <- ggplot(data = mapData) +
      geom_polygon_interactive(aes(long, lat,
        group = group,
        fill = prop,
        tooltip = tip, data_id = id
      )) +
      scale_fill_distiller(
        palette = color, direction = 1,
        name = NULL,
        labels = percent_format(accuracy = 1)
      ) +
      coord_map() +
      theme_void(base_size = baseSize) +
      theme(legend.position = legende)
    # bei unbewohnt/Fallzahl < smallN 2. Legende -------------------------------
    if (nrow(mapData %>% filter(is.na(prop))) > 0) {
      map <- map +
        geom_polygon_interactive(
          data = mapData %>% filter(is.na(prop)),
          aes(long, lat,
            group = group,
            tooltip = tip, data_id = id,
            colour = labGrau
          ),
          linetype = "blank", fill = "grey60"
        ) +
        guides(
          fill = guide_colourbar(order = 1),
          color = guide_legend(order = 2, title = NULL)
        )
    }
  }
  # ----------------------------------------------------------------------------
  # Legende (hier nur oben/unten) ----------------------------------------------
  if (legende == "bottom" || legende == "top") {
    if (scale != "continuous") {
      # map <- map +
      #   guides(fill = guide_legend(nrow = 1))
    } else {
      map <- map +
        guides(fill = guide_colourbar(order = 1, barwidth = 10))
    }
  }
  # Gewässer -------------------------------------------------------------------
  if (water == TRUE) {
    map <- map + waterLayer
  }
  # Grünflächen ----------------------------------------------------------------
  if (green == TRUE) {
    map <- map + greenLayer
  }
  # andere unbewohnte Flächen --------------------------------------------------
  if (other == TRUE) {
    map <- map + otherLayer
  }
  # Schulen --------------------------------------------------------------------
  if (schulen == TRUE) {
    # browser()
    map <- map + schulenLayer
    # nicht kontinuierliche Skalen:
    if (scale != "continuous") {
      map <- map + guides(fill = guide_legend(order = 1))
    } else {
      # kontinuierliche skala
      if (legende != "bottom" && legende != "top") {
        map <- map + guides(
          fill = guide_colourbar(order = 1),
          color = guide_legend(order = 2, title = NULL)
        )
      } else { # oben oder unten mit barwidth
        map <- map + guides(
          fill = guide_colourbar(order = 1, barwidth = 10),
          color = guide_legend(order = 2, title = NULL)
        )
      }
    }
  }
  # zum Schluss die Pfade darüberzeichnen --------------------------------------
  if (path == TRUE) {
    map <- map + pathLayer
  }
  map <- map + pathLayerMitte
  # Beschriftung: Wert ---------------------------------------------------------
  if (valueShow == TRUE) {
    valueData <- mapData %>%
      ungroup() %>%
      select(longCenter, latCenter, prop) %>%
      distinct()
    if (percentHide == FALSE) { # mit %-Zeichen
      map <- map +
        geom_text(
          data = valueData,
          aes(
            label = ifelse(!is.na(prop),
              paste0(round(prop * 100, 0), "%"), ""
            ),
            x = longCenter, y = latCenter
          ),
          size = geomTextSize, na.rm = TRUE
        )
    } else { # ohne %-Zeichen
      map <- map +
        geom_text(
          data = valueData, aes(
            label = round(prop * 100, 0),
            x = longCenter, y = latCenter
          ),
          size = geomTextSize, na.rm = TRUE
        )
    }
  }
  # ----------------------------------------------------------------------------
  return(map)
}
################################################################################
# Kerndichteplots erstellen: normale + Hotspot-Kernelheaping-Karte -------------
createKernelheapingPlot <- function(mapData,
                                    kData,
                                    hotspot = FALSE,
                                    baseSize = 12,
                                    legende = "right",
                                    color = "OrRd",
                                    path,
                                    schulen,
                                    water,
                                    green,
                                    other) {
  # ggplot aufbauen ------------------------------------------------------------
  # normale Kernelheaping-Karte
  if (hotspot == FALSE) {
    map <- ggplot(data = mapData) +
      geom_raster(data = kData, aes(long, lat, fill = Proportion)) +
      scale_fill_distiller(
        palette = color, direction = 1,
        name = NULL,
        labels = percent_format(accuracy = 1)
      ) +
      geom_polygon_interactive(aes(long, lat,
        group = group,
        tooltip = tip, data_id = id
      ),
      alpha = 1 / 100
      ) +
      coord_quickmap() +
      theme_void(base_size = baseSize) +
      theme(legend.position = legende)
  }
  # Hotspot Kernelheaping-Karte
  if (hotspot == TRUE) {
    # Quantile berechnen
    kDataQ <- kData %>%
      filter(Proportion > quantile(Proportion, hotspotQuantile)) %>%
      mutate(Proportion = "10% der Fläche mit höchsten Anteilswerten")
    map <- ggplot(data = mapData) +
      geom_raster(data = kDataQ, aes(long, lat, fill = Proportion)) +
      scale_fill_manual(values = brewer.pal(3, color)[3]) +
      geom_polygon_interactive(aes(long, lat,
        group = group,
        tooltip = tip, data_id = id
      ),
      alpha = 1 / 100
      ) +
      coord_quickmap() +
      theme_void(base_size = baseSize) +
      theme(legend.position = "none")
  }
  # bei unbewohnt/Fallzahl < smallN grau darstellen ----------------------------
  if (nrow(mapData %>% filter(is.na(prop))) > 0) {
    map <- map +
      geom_polygon_interactive(
        data = mapData %>% filter(is.na(prop)),
        aes(long, lat,
          group = group,
          tooltip = tip, data_id = id,
          colour = labGrau
        ),
        linetype = "blank", fill = "grey60"
      ) +
      guides(
        fill = guide_colourbar(order = 1),
        color = guide_legend(order = 2, title = NULL)
      )
  }
  # Legende --------------------------------------------------------------------
  # nicht bei Hotspot-Karten
  if (hotspot == FALSE) {
    if (legende == "bottom" || legende == "top") {
      map <- map +
        guides(fill = guide_colourbar(barwidth = 10))
    }
  }
  # Gewässer -------------------------------------------------------------------
  if (water == TRUE) {
    map <- map + waterLayer
  }
  # Grünflächen ----------------------------------------------------------------
  if (green == TRUE) {
    map <- map + greenLayer
  }
  # andere unbewohnte Flächen --------------------------------------------------
  if (other == TRUE) {
    map <- map + otherLayer
  }
  # Schulen --------------------------------------------------------------------
  if (schulen == TRUE) {
    map <- map + schulenLayer
    if (legende != "bottom" && legende != "top") {
      map <- map + guides(fill = guide_colourbar(order = 1))
    } else { # Legende oben oder unten mit barwidth
      map <- map + guides(fill = guide_colourbar(order = 1, barwidth = 10))
    }
  }
  # zum Schluss die Pfade darüberzeichnen --------------------------------------
  if (path == TRUE) {
    map <- map + pathLayer
  }
  map <- map + pathLayerMitte
  # ----------------------------------------------------------------------------
  return(map)
}
################################################################################
