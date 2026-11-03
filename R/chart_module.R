# A chart and its download button. The plot and the button are placed
# separately in the UI, so there are two UI functions.

chart_plot_ui <- function(id) {
  plotOutput(NS(id, "plot"))
}

chart_download_ui <- function(id, label) {
  downloadButton(NS(id, "download"), label = label)
}

#' @param chart a reactive returning the chart, or, if there is none, a
#'   condition whose message says why
#' @param filename a reactive returning the download's file name
#' @param height height of the downloaded image, in inches
chart_server <- function(id, chart, filename, height = 5) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      if (inherits(chart(), "condition")) {
        validate(conditionMessage(chart()))
      }
      print(chart())
    })

    output$download <- downloadHandler(
      filename = function() filename(),
      content = function(file) {
        req(!inherits(chart(), "condition"))
        png(file, width = 10, height = height, units = "in", res = 300)
        print(chart())
        dev.off()
      }
    )
  })
}
