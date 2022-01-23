################################################################################
# ESU explorer
#
# functionsKernelheaping.R
# Ulrike Niemann
#
################################################################################
#
# die Funkionen aus dem Kernelheaping-Package insofern modifizieren,
# dass eine Fortschrittsanzeige möglich ist
# außerdem siehe unten: Fehlerbehandlung für n=1 bei sample()
#
custom_dshapebivrProp <- function(data,
                                  burnin = 2,
                                  samples = 5,
                                  adaptive = FALSE,
                                  shapefile,
                                  gridsize = 200,
                                  boundary = FALSE,
                                  deleteShapes = NULL,
                                  fastWeights = TRUE,
                                  numChains = 1,
                                  numThreads = 1) # numThreads immer 1, shiny kann sonst keinen Fortschritt anzeigen
{
  pol.x <- list()
  pol.y <- list()
  for (i in 1:length(shapefile@polygons)) {
    pol.x[[i]] <- shapefile@polygons[[i]]@Polygons[[1]]@coords[
      ,
      1
    ]
    pol.y[[i]] <- shapefile@polygons[[i]]@Polygons[[1]]@coords[
      ,
      2
    ]
  }
  npointsAll <- data[, 4]
  npoints <- data[, 3]
  if (length(gridsize) == 1) {
    gridsize <- c(gridsize, gridsize)
  }
  gridx <- seq(shapefile@bbox[1, 1], shapefile@bbox[1, 2],
    length = gridsize[1]
  )
  gridy <- seq(shapefile@bbox[2, 1], shapefile@bbox[2, 2],
    length = gridsize[2]
  )
  grid <- as.matrix(expand.grid(gridx, gridy))
  MestimatesAll <- ks::kde(x = data[, c(1, 2)], H = 10 * diag(c(diff(shapefile@bbox[1, ]) / sqrt(length(shapefile@polygons)), c(diff(shapefile@bbox[2, ]) / sqrt(length(shapefile@polygons)))))^2, gridsize = c(
    length(gridx),
    length(gridy)
  ), xmin = c(min(gridx), min(gridy)), xmax = c(
    max(gridx),
    max(gridy)
  ), w = nrow(data) * data[, 4] / sum(data[, 4]))
  Mestimates <- ks::kde(x = data[, c(1, 2)], H = 10 * diag(c(diff(shapefile@bbox[1, ]) / sqrt(length(shapefile@polygons)), c(diff(shapefile@bbox[2, ]) / sqrt(length(shapefile@polygons)))))^2, gridsize = c(
    length(gridx),
    length(gridy)
  ), xmin = c(min(gridx), min(gridy)), xmax = c(
    max(gridx),
    max(gridy)
  ), w = nrow(data) * data[, 3] / sum(data[, 3]))
  resultDensity <- array(dim = c(
    numChains, burnin + samples,
    length(gridx), length(gridy)
  ))
  resultX <- array(dim = c(
    numChains, samples + burnin, sum(npoints),
    2
  ))
  proportionArray <- array(dim = c(
    numChains, burnin + samples,
    length(gridx), length(gridy)
  ))
  printTiming("Calc selectionGrid", {
    selectionGrid <- lapply(1:nrow(data), function(i) {
      lapply(
        1:length(shapefile@polygons[[i]]@Polygons),
        function(j) {
          if (shapefile@polygons[[i]]@Polygons[[j]]@hole ==
            FALSE) {
            which(point.in.polygon(grid[, 1], grid[
              ,
              2
            ], shapefile@polygons[[i]]@Polygons[[j]]@coords[
              ,
              1
            ], shapefile@polygons[[i]]@Polygons[[j]]@coords[
              ,
              2
            ]) == 1)
          }
        }
      ) %>% unlist()
    })
  })
  # Fortschritt
  incProgress(1 / (numChains * (burnin + samples) + 5)) ###
  printTiming("Calc selectionGridHole", {
    selectionGridHole <- lapply(1:nrow(data), function(i) {
      lapply(
        1:length(shapefile@polygons[[i]]@Polygons),
        function(j) {
          if (shapefile@polygons[[i]]@Polygons[[j]]@hole ==
            TRUE) {
            which(point.in.polygon(grid[, 1], grid[
              ,
              2
            ], shapefile@polygons[[i]]@Polygons[[j]]@coords[
              ,
              1
            ], shapefile@polygons[[i]]@Polygons[[j]]@coords[
              ,
              2
            ]) == 1)
          }
        }
      ) %>% unlist()
    })
  })
  printTiming("Recalc selectionGrid", {
    selectionGrid <- lapply(1:length(selectionGrid), function(x) setdiff(
        selectionGrid[[x]],
        selectionGridHole[[x]]
      ))
    outside <- grid[-unlist(selectionGrid), ]
  })
  # Fortschritt
  incProgress(1 / (numChains * (burnin + samples) + 5)) ###
  printTiming("Consider DeleteShapes", {
    unselectionGrid <- NULL
    unselectionGridHoles <- NULL
    if (!is.null(deleteShapes)) {
      unselectionGrid <- lapply(
        1:length(deleteShapes@polygons),
        function(i) {
          lapply(
            1:length(deleteShapes@polygons[[i]]@Polygons),
            function(j) {
              if (deleteShapes@polygons[[i]]@Polygons[[j]]@hole ==
                FALSE) {
                which(point.in.polygon(grid[, 1], grid[
                  ,
                  2
                ], deleteShapes@polygons[[i]]@Polygons[[j]]@coords[
                  ,
                  1
                ], deleteShapes@polygons[[i]]@Polygons[[j]]@coords[
                  ,
                  2
                ]) == 1)
              }
            }
          ) %>% unlist()
        }
      ) %>% unlist()
      unselectionGridHoles <- lapply(
        1:length(deleteShapes@polygons),
        function(i) {
          lapply(
            1:length(deleteShapes@polygons[[i]]@Polygons),
            function(j) {
              if (deleteShapes@polygons[[i]]@Polygons[[j]]@hole ==
                TRUE) {
                which(point.in.polygon(grid[, 1], grid[
                  ,
                  2
                ], deleteShapes@polygons[[i]]@Polygons[[j]]@coords[
                  ,
                  1
                ], deleteShapes@polygons[[i]]@Polygons[[j]]@coords[
                  ,
                  2
                ]) == 1)
              }
            }
          ) %>% unlist()
        }
      ) %>% unlist()
      unselectionGrid <- setdiff(unselectionGrid, unselectionGridHoles)
      outside <- grid[-setdiff(
        unlist(selectionGrid),
        unselectionGrid
      ), ]
      selectionGrid <- lapply(selectionGrid, function(x) setdiff(
          x,
          unselectionGrid
        ))
    }
  })
  inside <- grid[unlist(selectionGrid), ]
  for (j in which(sapply(selectionGrid, length) == 0)) {
    selectionGrid[[j]] <- which.min(colSums((t(grid) - as.numeric(data[
      j,
      c(1, 2)
    ]))^2))
  }
  rm(selectionGridHole, pol.x, pol.y)
  gc()
  numCoresToUse <- min(numThreads, numChains, detectCores())
  # Fortschritt
  incProgress(1 / (numChains * (burnin + samples) + 5)) ###
  if (numCoresToUse == 1) {
    # bei Berechnung auf einem CPU-Kern:
    for (c in seq(1, numChains)) {
      # in diese Funktion die Anzahl numChains mit als Parameter
      res <- dshapebivrProp_calcChain(
        c, NA, MestimatesAll,
        Mestimates, burnin, samples, grid, gridx, gridy,
        selectionGrid, shapefile, npointsAll, npoints,
        adaptive, boundary, fastWeights, data, inside,
        outside, deleteShapes, unselectionGrid, numChains
      )
      resultDensity[c, , , ] <- res$resultDensity
      if (isOutputlevelHigh()) {
        resultX[c, , , ] <- res$resultX
      }
      proportionArray[c, , , ] <- res$proportionArray
      Mestimates <- res$Mestimates
      rm(res)
      gc()
    }
  }
  else { # numThreads immer 1, shiny kann sonst keinen Fortschritt anzeigen
    # cl <- makeCluster(numCoresToUse, type = "PSOCK", outfile = "")
    # clusterEvalQ(cl, {
    #   library(ks)
    #   library(mvtnorm)
    #   library(dplyr)
    #   library(fastmatch)
    #   library(Kernelheaping)
    # })
    # baseFilename = paste(tempdir(), "/", runif(1, min = 1e+06,
    #                                            max = 9999999), "_", sep = "")
    # parLapply(cl, 1:numChains, dshapebivrProp_calcChain,
    #           baseFilename, MestimatesAll, Mestimates, burnin,
    #           samples, grid, gridx, gridy, selectionGrid, shapefile,
    #           npointsAll, npoints, adaptive, boundary, fastWeights,
    #           data, inside, outside, deleteShapes, unselectionGrid)
    # stopCluster(cl)
    # closeAllConnections()
    # rm(cl)
    # gc()
    # for (c in seq(1, numChains)) {
    #   ret <- NULL
    #   load(paste(baseFilename, c, sep = ""))
    #   resultDensity[c, , , ] <- ret$resultDensity
    #   if (isOutputlevelHigh()) {
    #     resultX[c, , , ] <- ret$resultX
    #   }
    #   proportionArray[c, , , ] <- ret$proportionArray
    #   Mestimates <- ret$Mestimates
    #   rm(ret)
    #   gc()
    # }
  }
  indexGridColumns <- c(length(dim(resultDensity[, -c(1:burnin), , ])) - 1, length(dim(resultDensity[, -c(1:burnin), , ])))
  Mestimates$estimate <- apply(resultDensity[, -c(1:burnin), , ], indexGridColumns, mean)
  est <- list(
    Mestimates = Mestimates, resultDensity = resultDensity,
    data = data, gridx = gridx, gridy = gridy, shapefile = shapefile,
    burnin = burnin, samples = samples, adaptive = adaptive,
    numChains = numChains, proportions = apply(proportionArray[
      ,
      -c(1:burnin), ,
    ], indexGridColumns, mean)
  )
  if (isOutputlevelHigh()) {
    est[["resultX"]] <- resultX
  }
  class(est) <- "bivshape"
  # Fortschritt
  incProgress(1 / (numChains * (burnin + samples) + 5)) ###
  return(est)
}
################################################################################
# hier numChain als zusätzlichen Parameter (letzter)
custom_dshapebivrProp_calcChain <- function(chain,
                                            saveAsBaseFileName,
                                            MestimatesAll,
                                            Mestimates,
                                            burnin,
                                            samples,
                                            grid,
                                            gridx,
                                            gridy,
                                            selectionGrid,
                                            shapefile,
                                            npointsAll,
                                            npoints,
                                            adaptive,
                                            boundary,
                                            fastWeights,
                                            data,
                                            inside,
                                            outside,
                                            deleteShapes,
                                            unselectionGrid,
                                            numChains = 1) {
  # printInfo("Start Chain ", chain)
  # Fortschritt
  incProgress(1 / (numChains * (burnin + samples) + 5)) ###
  ret <- list()
  ret$resultDensity <- array(dim = c(
    burnin + samples, length(gridx),
    length(gridy)
  ))
  ret$resultX <- array(dim = c(
    samples + burnin, sum(npoints),
    2
  ))
  ret$proportionArray <- array(dim = c(
    burnin + samples, length(gridx),
    length(gridy)
  ))
  Max_2SingleID <- max(grid)
  inside_2SingleID <- apply(inside, 1, function(x) {
    x[1] * Max_2SingleID + x[2]
  })
  for (j in 1:(burnin + samples)) {
    newAll <- matrix(nrow = sum(npointsAll), ncol = 2)
    newAllCnt <- 1
    new <- matrix(nrow = sum(npoints), ncol = 2)
    newCnt <- 1
    # print(j)
    printTiming("Calc Probabilities", {
      for (i in 1:length(selectionGrid)) {
        probsAll <- MestimatesAll$estimate[cbind(match(grid[
          selectionGrid[[i]],
          1
        ], MestimatesAll$eval.points[[1]]), match(grid[
          selectionGrid[[i]],
          2
        ], MestimatesAll$eval.points[[2]]))] + 1e-16
        probs <- Mestimates$estimate[cbind(match(grid[
          selectionGrid[[i]],
          1
        ], Mestimates$eval.points[[1]]), match(grid[
          selectionGrid[[i]],
          2
        ], Mestimates$eval.points[[2]]))] + 1e-16
        probsAll <- probsAll / sum(probsAll)
        probs <- probs / sum(probs)
        probsPROP <- (probs / probsAll) * (sum(data[
          i,
          3
        ]) / sum(data[i, 4]))
        points <- matrix(ncol = 2, grid[selectionGrid[[i]], ])
        if (length(selectionGrid[[i]]) == 0) {
          points <- matrix(ncol = 2, shapefile@polygons[[i]]@labpt)
          probs <- 1
          probsAll <- 1
        }
        if (npointsAll[i] > 0) {
          sampleAll <- sample(1:nrow(points),
            size = max(0,
              npointsAll[i],
              na.rm = T
            ), replace = T,
            prob = probsAll
          )
          newAll[newAllCnt:(newAllCnt + npointsAll[i] -
            1), ] <- points[sampleAll, ]
          newAllCnt <- newAllCnt + npointsAll[i]
        }
        if (npoints[i] > 0) {
          # browser()
          # bei Länge sampleAll = 1 versucht sample() das als Vektor zu sehen,
          # Error wg. unpassenden probs
          # documentation sample()
          # If x has length 1, is numeric (in the sense of is.numeric) and x >= 1,
          # sampling via sample takes place from 1:x.
          # Fehlerbehandlung:
          if (length(sampleAll) == 1) {
            sampleProp <- sampleAll
          } else {
            # Ende Fehlerbehandlung
            sampleProp <- sample(sampleAll, size = max(0,
              npoints[i],
              na.rm = T
            ), prob = probsPROP[sampleAll])
          }
          new[newCnt:(newCnt + npoints[i] - 1), ] <- points[sampleProp, ]
          newCnt <- newCnt + npoints[i]
        }
      }
    })
    printTiming("Recompute H", {
      if (adaptive == FALSE) {
        H <- ks::Hpi(x = new, binned = TRUE) * 2
      }
      if (adaptive == TRUE) {
        H <- ks::Hpi(x = new, binned = TRUE)
        H <- sqrt(sqrt(H[1, 1] * H[2, 2]))
      }
    })
    wAll <- rep(1, nrow(newAll))
    w <- rep(1, nrow(new))
    if (boundary == TRUE) {
      printTiming("Calc Weights", {
        if (fastWeights == FALSE || j <= ceiling(0.1 *
          (burnin + samples))) {
          weights <- calcWeights_fast(
            inside = inside,
            outside = outside, gridx = gridx, gridy = gridy,
            H = H
          )
        }
      })
      printTiming("Match Weights", {
        new_2SingleID <- apply(new, 1, function(x) {
          x[1] * Max_2SingleID + x[2]
        })
        newAll_2SingleID <- apply(newAll, 1, function(x) {
          x[1] * Max_2SingleID + x[2]
        })
        w <- weights[fmatch(new_2SingleID, inside_2SingleID,
          nomatch = 1
        )]
        wAll <- weights[fmatch(newAll_2SingleID, inside_2SingleID,
          nomatch = 1
        )]
      })
    }
    printTiming("Recompute Density", {
      if (adaptive == FALSE) {
        MestimatesAll <- ks::kde(
          x = newAll, H = H,
          gridsize = c(length(gridx), length(gridy)),
          bgridsize = c(length(gridx), length(gridy)),
          xmin = c(min(gridx), min(gridy)), xmax = c(
            max(gridx),
            max(gridy)
          ), binned = TRUE, w = wAll / mean(wAll)
        )
        Mestimates <- ks::kde(
          x = new, H = H, gridsize = c(
            length(gridx),
            length(gridy)
          ), bgridsize = c(
            length(gridx),
            length(gridy)
          ), xmin = c(min(gridx), min(gridy)),
          xmax = c(max(gridx), max(gridy)), binned = TRUE,
          w = w / mean(w)
        )
      }
      if (adaptive == TRUE) {
        counts <- plyr::count(new)
        MestimatesAd <- sparr::bivariate.density(
          data = counts[
            ,
            c(1:2)
          ], pilotH = H, res = length(gridx),
          xrange = range(gridx), yrange = range(gridy),
          adaptive = TRUE, comment = FALSE, counts = counts[
            ,
            3
          ]
        )
        Mestimates$estimate <- MestimatesAd$Zm
      }
      Mestimates$estimate[Mestimates$estimate < 0] <- 0
      MestimatesAll$estimate[MestimatesAll$estimate <
        0] <- 0
    })
    printTiming("Delete Shapes", {
      if (!is.null(deleteShapes)) {
        Mestimates$estimate[-setdiff(
          unlist(selectionGrid),
          unselectionGrid
        )] <- 0
        MestimatesAll$estimate[-setdiff(
          unlist(selectionGrid),
          unselectionGrid
        )] <- 0
      }
      else {
        Mestimates$estimate[-(unlist(selectionGrid))] <- 0
        MestimatesAll$estimate[-(unlist(selectionGrid))] <- 0
      }
    })
    printTiming("Match Density", {
      densityAll <- matrix(NA,
        ncol = ncol(Mestimates$estimate),
        nrow = nrow(Mestimates$estimate)
      )
      densityPart <- matrix(NA,
        ncol = ncol(Mestimates$estimate),
        nrow = nrow(Mestimates$estimate)
      )
      densityAll[cbind(match(grid[
        selectionGrid %>% unlist(),
        1
      ], MestimatesAll$eval.points[[1]]), match(grid[selectionGrid %>%
        unlist(), 2], MestimatesAll$eval.points[[2]]))] <- MestimatesAll$estimate[cbind(
        match(grid[selectionGrid %>%
          unlist(), 1], MestimatesAll$eval.points[[1]]),
        match(grid[selectionGrid %>% unlist(), 2], MestimatesAll$eval.points[[2]])
      )]
      densityPart[cbind(match(grid[
        selectionGrid %>% unlist(),
        1
      ], MestimatesAll$eval.points[[1]]), match(grid[selectionGrid %>%
        unlist(), 2], MestimatesAll$eval.points[[2]]))] <- Mestimates$estimate[cbind(
        match(grid[selectionGrid %>%
          unlist(), 1], MestimatesAll$eval.points[[1]]),
        match(grid[selectionGrid %>% unlist(), 2], MestimatesAll$eval.points[[2]])
      )]
    })
    printTiming("Assignments", {
      densityPart <- densityPart / sum(densityPart, na.rm = TRUE)
      densityAll <- densityAll / sum(densityAll, na.rm = TRUE)
      densityAll <- densityAll + 1e-96
      proportion <- (densityPart / densityAll) * (sum(data[
        ,
        3
      ]) / sum(data[, 4]))
      proportion[proportion < 0] <- 0
      proportion[proportion > 1] <- 1
      Mestimates$estimate[is.na(Mestimates$estimate)] <- 1e-96
      ret$resultDensity[j, , ] <- Mestimates$estimate
      ret$proportionArray[j, , ] <- proportion
      ret$resultX[j, , ] <- new
    })
    # printInfo("Iteration: ", j, " of ", burnin + samples,
    #           " in chain ", chain)
    # Fortschritt
    incProgress(1 / (numChains * (burnin + samples) + 5)) ###
  }
  ret$Mestimates <- Mestimates
  if (is.na(saveAsBaseFileName)) {
    return(ret)
  }
  else {
    save(ret,
      file = paste(saveAsBaseFileName, chain, sep = ""),
      envir = environment()
    )
    return(NA)
  }
}
###### jetzt zuweisen ----------------------------------------------------------
environment(custom_dshapebivrProp) <- asNamespace("Kernelheaping")
# assignInNamespace("dshapebivrProp",
#                   custom_dshapebivrProp, ns = "Kernelheaping")
#
environment(custom_dshapebivrProp_calcChain) <- asNamespace("Kernelheaping")
assignInNamespace("dshapebivrProp_calcChain",
  custom_dshapebivrProp_calcChain,
  ns = "Kernelheaping"
)
################################################################################
