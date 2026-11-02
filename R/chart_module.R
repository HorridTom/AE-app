# A chart and its download button. The plot and the button are placed
# separately in the UI, so there are two UI functions.

chart_plot_ui <- function(id) {
  plotOutput(NS(id, "plot"))
}

chart_download_ui <- function(id, label) {
  downloadButton(NS(id, "download"), label = label)
}

#' @param chart a reactive returning the chart, or NULL if there is none
#' @param filename a reactive returning the download's file name
#' @param height height of the downloaded image, in inches
chart_server <- function(id, chart, filename, height = 5) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      validate(need(
        !is.null(chart()),
        "There is no data to chart for this selection."
      ))
      print(chart())
    })

    output$download <- downloadHandler(
      filename = function() filename(),
      content = function(file) {
        png(file, width = 10, height = height, units = "in", res = 300)
        print(chart())
        dev.off()
      }
    )
  })
}
