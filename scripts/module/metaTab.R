################################################################################
# ESU explorer
#
# metaTab.R (module, Seite "Variablen Infos")
# Ulrike Niemann
#
################################################################################
# Module UI function:
metaTabUI <- function(id) {
  ns <- shiny::NS(id)
  #
  tagList(
    "Im Suche-Feld rechts können Sie dynamisch nach Inhalten suchen.",
    br(),
    "Durch klicken auf das grüne Plus-Symbol erhalten Sie weitere Informationen
    zur entsprechenden Variable.",
    br(), br(),
    DT::dataTableOutput(ns("meta"))
  )
}
#
################################################################################
# Module server function:
metaTab <- function(input, output, session) {
  output$meta <- renderDT(
    DT::datatable(
      cbind(" " = "+", metaTable),
      escape = F,
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 5:9)), # ausgeblendete Spalten (js: start 0!)
          list(orderable = FALSE, className = "details-control", targets = 1)
        ),
        # Sprache anpassen
        language = list(
          search = "Suche:",
          lengthMenu = "Zeige _MENU_ Variablen",
          info = "_START_ bis _END_ von _TOTAL_ Variablen",
          paginate = list(previous = "Zurück", `next` = "Weiter"),
          sInfoFiltered = "(gefiltert von _MAX_ Einträgen)",
          sZeroRecords = "Keine Einträge vorhanden"
        )
      ),
      selection = "none", # Zeilen können nicht ausgewählt werden
      callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> ' +
                '<b>Herkunft:</b> ' + 
                d[7] + 
                '</br><b>Ausprägungen:</b> ' + 
                d[9] + 
                '</br><b>Meßniveau:</b> ' + 
                d[6] + 
                '</br><b>Zusatzinformationen:</b> ' + 
                d[8] + 
                '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                td.html('+');
                td.css('background-color','#31b131');
                } else {
                row.child(format(row.data())).show();
                td.html('-');
                td.css('background-color','red');
                }
                });")
    )
  )
}
#
################################################################################
