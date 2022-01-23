################################################################################
# ESU explorer
#
# downloadDoku.R (module, Seite "Dokumentationsbögen")
# Ulrike Niemann
#
################################################################################
# Module UI function:
downloadBogenUI <- function(id) {
  ns <- shiny::NS(id)
  #
  tagList(
    downloadButton(ns("bogen_2019"), "Export Dokumentationsbogen 2019 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2018"), "Export Dokumentationsbogen 2018 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2017"), "Export Dokumentationsbogen 2017 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2016"), "Export Dokumentationsbogen 2016 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2015"), "Export Dokumentationsbogen 2015 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2014"), "Export Dokumentationsbogen 2014 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2013"), "Export Dokumentationsbogen 2013 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2012"), "Export Dokumentationsbogen 2012 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2011"), "Export Dokumentationsbogen 2011 (pdf)"),
    br(), br(),
    downloadButton(ns("bogen_2010"), "Export Dokumentationsbogen 2010 (pdf)"),
    br(), br()
  )
}
#
################################################################################
# Module server function:
downloadBogen <- function(input, output, session, year) {
  download <- function(year) {
    downloadHandler(
      filename = function() {
        paste0("Dokumentationsbogen ESU ", year, ".pdf")
      },
      content = function(file) {
        file.copy(paste0("download/Dokumentationsbogen ESU ", year, ".pdf"), 
                  file)
      },
      contentType = "application/pdf"
    ) # Ende downloadHandler
  }
  output$bogen_2010 <- download(year = "2010")
  output$bogen_2011 <- download(year = "2011")
  output$bogen_2012 <- download(year = "2012")
  output$bogen_2013 <- download(year = "2013")
  output$bogen_2014 <- download(year = "2014")
  output$bogen_2015 <- download(year = "2015")
  output$bogen_2016 <- download(year = "2016")
  output$bogen_2017 <- download(year = "2017")
  output$bogen_2018 <- download(year = "2018")
  output$bogen_2019 <- download(year = "2019")
}
#
################################################################################
