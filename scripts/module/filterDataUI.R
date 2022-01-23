################################################################################
# ESU explorer
#
# filterDataUI.R (integriert in module "analysis" und "maps")
# Ulrike Niemann
#
################################################################################
# Module UI function:
filterDatenUI <- function(id) {
  ns <- shiny::NS(id)
  #
  tagList(
    # ##########################################################################
    fluidRow(
      div(
        id = ns("filterSettings"),
        box(
          title = "Erweiterte Filter-Einstellungen",
          width = 12, status = "primary", solidHeader = FALSE,
          collapsible = TRUE, collapsed = TRUE,
          "Bis zu drei Filter wählen:", br(), "", br(),
          #### 1. Filter -------------------------------------------------------
          fluidRow(
            column(4, div(
              class = "selectSmall",
              selectizeInput(
                ns("filter1"),
                label = NULL,
                choices = c(
                  "Filtervariable auswählen" = "",
                  varsBereiche[-1] # ohne Gesamt
                ), 
                selected = ""
              )
            )),
            column(2, div(
              class = "selectSmall",
              selectizeInput(ns("operatorFilter1"),
                label = NULL,
                choices = c("gleich", "ungleich"),
                selected = "gleich"
              )
            )),
            column(4, div(
              class = "selectSmall",
              selectizeInput(ns("valueFilter1"),
                label = NULL,
                choices = c("Eine oder mehr Kategorien wählen" = ""),
                selected = "",
                multiple = TRUE
              )
            )),
            column(
              2,
              hidden(span(
                id = ns("divDeleteFilter1"),
                class = "filterbutton resetFilter",
                title = "Eingaben Filter 1 löschen",
                actionButton(
                  ns("deleteFilter1"), "",
                  icon("minus-circle")
                )
              )),
              hidden(span(
                id = ns("divZweitFilter"),
                class = "filterbutton newFilter",
                title = "Zweiten Filter anlegen",
                actionButton(
                  ns("zweitFilter"), "",
                  icon("plus-circle")
                )
              ))
            )
          ),
          #### 2. Filter -------------------------------------------------------
          fluidRow(
            column(4, div(class = "selectSmall", hidden(
              selectizeInput(ns("filter2"),
                label = NULL,
                choices = c(
                  "Filtervariable auswählen" = "",
                  varsBereiche[-1] # ohne Gesamt
                ), 
                selected = ""
              )
            ))),
            column(2, div(class = "selectSmall", hidden(
              selectizeInput(ns("operatorFilter2"),
                label = NULL,
                choices = c("gleich", "ungleich"),
                selected = "gleich"
              )
            ))),
            column(4, div(class = "selectSmall", hidden(
              selectizeInput(ns("valueFilter2"),
                label = NULL,
                choices = c("Eine oder mehr Kategorien wählen" = ""),
                selected = "",
                multiple = TRUE
              )
            ))),
            column(
              2,
              hidden(span(
                id = ns("divDeleteFilter2"),
                class = "filterbutton resetFilter",
                title = "Eingaben Filter 2 löschen",
                actionButton(
                  ns("deleteFilter2"), "",
                  icon("minus-circle")
                )
              )),
              hidden(span(
                id = ns("divDrittFilter"),
                class = "filterbutton newFilter",
                title = "Dritten Filter anlegen",
                actionButton(
                  ns("drittFilter"), "",
                  icon("plus-circle")
                )
              ))
            )
          ),
          #### 3. Filter -------------------------------------------------------
          fluidRow(
            column(4, div(class = "selectSmall", hidden(
              selectizeInput(ns("filter3"),
                label = NULL,
                choices = c(
                  "Filtervariable auswählen" = "",
                  varsBereiche[-1] # ohne Gesamt
                ), 
                selected = ""
              )
            ))),
            column(2, div(class = "selectSmall", hidden(
              selectizeInput(ns("operatorFilter3"),
                label = NULL,
                choices = c("gleich", "ungleich"),
                selected = "gleich"
              )
            ))),
            column(4, div(class = "selectSmall", hidden(
              selectizeInput(ns("valueFilter3"),
                label = NULL,
                choices = c("Eine oder mehr Kategorien wählen" = ""),
                selected = "",
                multiple = TRUE
              )
            ))),
            column(
              2,
              hidden(span(
                id = ns("divDeleteFilter3"),
                class = "filterbutton resetFilter",
                title = "Eingaben Filter 3 löschen",
                actionButton(
                  ns("deleteFilter3"), "",
                  icon("minus-circle")
                )
              ))
            )
          ),
          #### Filter-Button und Textinfo zum Filter ---------------------------
          div(
            style = "margin:10px 0px",
            hidden(actionButton(ns("filterDo"),
              "Filter anwenden",
              icon = icon("play")
            ))
          ),
          div(hidden(actionButton(ns("resetFilter"),
            "Filter zurücksetzen",
            icon = icon("undo-alt")
          ))),
          uiOutput(ns("infoFilter"))
        )
      )
    )
  )
}
################################################################################
