# A chart and its download button. The plot and the button are placed
# separately in the UI, so there are two UI functions.

chart_plot_ui <- function(id) {
  # The height is set in chart_server(), to suit the width
  plotOutput(NS(id, "plot"), height = "auto")
}

chart_download_ui <- function(id, label) {
  downloadButton(NS(id, "download"), label = label)
}

#' @param chart a reactive returning the chart, or, if there is none, a
#'   condition whose message says why
#' @param filename a reactive returning the download's file name
#' @param caption a reactive returning the caption for the download
#' @param height height of the downloaded image, in inches
chart_server <- function(id, chart, filename, caption, height = 6) {
  moduleServer(id, function(input, output, session) {
    # The width of the plot on the user's screen, in pixels
    plot_width <- function() {
      session$clientData[[paste0("output_", session$ns("plot"), "_width")]]
    }

    # Log the times of the session's first chart (see R/startup.R). A chart
    # can be tried more than once before it is drawn (e.g. the volume charts
    # wait for the first flush), so each step is logged only the first time.
    logged <- character()
    log_once <- function(step) {
      if (!step %in% logged) {
        log_timing(paste(id, "chart:", step))
        logged <<- c(logged, step)
      }
    }

    output$plot <- renderPlot(
      {
        log_once("analysing")
        if (inherits(chart(), "condition")) {
          validate(conditionMessage(chart()))
        }
        log_once("drawing")
        draw_chart(fit_to_width(chart(), plot_width()))
        log_once("drawn")
      },
      height = function() {
        if (is_narrow(plot_width())) 560 else 400
      }
    )

    output$download <- downloadHandler(
      filename = function() filename(),
      content = function(file) {
        req(!inherits(chart(), "condition"))
        png(file, width = 9, height = height, units = "in", res = 300)
        draw_chart(chart(), caption = caption())
        dev.off()
      }
    )
  })
}
