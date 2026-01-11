# app.R
# Dynamic clustering with "Go" to incorporate newly-added points
# Modified from shiny-examples/040-dynamic-clustering
library(shiny)
library(mclust)

ui <- fluidPage(
  titlePanel("Dynamic Clustering (modified)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Go (re-cluster)"),
      actionButton("clear", "Clear Points"),
      br(), br(),
      strong("Instructions:"),
      tags$div("Click on the plot to add points. The first clustering is performed automatically when there are at least 2 points."),
      tags$div("After clustering, newly added points are shown as unclustered (black open circles). Click 'Go' to cluster all points.")
    ),
    mainPanel(
      plotOutput("clusterPlot", height = "600px", click = "plot_click"),
      br(),
      verbatimTextOutput("numPoints")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    x = numeric(),
    y = numeric(),
    fit = NULL,         # last Mclust fit
    lastClusterN = 0    # number of points used in last clustering
  )

  # Add points on plot click
  observeEvent(input$plot_click, {
    click <- input$plot_click
    isolate({
      rv$x <- c(rv$x, click$x)
      rv$y <- c(rv$y, click$y)
    })
  })

  # Clear everything
  observeEvent(input$clear, {
    rv$x <- numeric()
    rv$y <- numeric()
    rv$fit <- NULL
    rv$lastClusterN <- 0
  })

  # Automatic initial clustering: if we have >= 2 points and we have never clustered before,
  # attempt to cluster automatically and treat that as the "initial clustering".
  observe({
    n <- length(rv$x)
    if (n >= 2 && rv$lastClusterN == 0) {
      data <- cbind(rv$x, rv$y)
      fit_try <- try(Mclust(data), silent = TRUE)
      if (!inherits(fit_try, "try-error") && !is.null(fit_try$classification)) {
        rv$fit <- fit_try
        rv$lastClusterN <- n
      }
    }
  })

  # "Go" button: cluster all current points (including newly added ones)
  observeEvent(input$go, {
    n <- length(rv$x)
    if (n < 2) {
      # nothing to cluster
      return()
    }
    data <- cbind(rv$x, rv$y)
    fit_try <- try(Mclust(data), silent = TRUE)
    if (!inherits(fit_try, "try-error") && !is.null(fit_try$classification)) {
      rv$fit <- fit_try
      rv$lastClusterN <- n
    } else {
      # If clustering fails, keep previous fit but don't update lastClusterN
      showNotification("Clustering failed: leaving previous clustering in place", type = "warning")
    }
  })

  output$numPoints <- renderText({
    paste0("Points: ", length(rv$x), 
           "   (Points used in last clustering: ", rv$lastClusterN, ")")
  })

  output$clusterPlot <- renderPlot({
    n <- length(rv$x)
    par(mar = c(4, 4, 2, 2))
    xlim <- c(-2, 2)
    ylim <- c(-2, 2)

    # If we have a previous clustering, draw clustered points (only those used in the clustering)
    if (!is.null(rv$fit) && rv$lastClusterN >= 2) {
      # Data used for the last clustering
      clustered_idx <- seq_len(rv$lastClusterN)
      data_clustered <- cbind(rv$x[clustered_idx], rv$y[clustered_idx])
      classification <- rv$fit$classification[clustered_idx]

      # Use mclust2Dplot to draw clusters for the clustered subset
      tryCatch({
        mclust2Dplot(data = data_clustered,
                     classification = classification,
                     what = "classification",
                     main = FALSE,
                     xlim = xlim,
                     ylim = ylim)
      }, error = function(e) {
        # Fallback: basic plot if mclust2Dplot fails
        plot(data_clustered, xlim = xlim, ylim = ylim, pch = 19, col = classification, xlab = "", ylab = "")
      })

      # If new points were added since last clustering, draw them as black open circles
      if (n > rv$lastClusterN) {
        new_idx <- (rv$lastClusterN + 1):n
        points(rv$x[new_idx], rv$y[new_idx], pch = 1, col = "black", cex = 1.2)
      }

      # If points have been added since last clustering, show the instruction text on the plot
      if (n > rv$lastClusterN) {
        txt <- "Points have been added since the last clustering.\nClick 'Go' to incorporate these new points in a new clustering."
        # place near bottom of plot area
        text_x <- mean(xlim)
        text_y <- min(ylim) + 0.12 * diff(ylim)
        text(text_x, text_y, labels = txt, col = "black", cex = 1.0)
      }
    } else {
      # No clustering yet (or less than 2 points used in last clustering) -> just plot points or instructions
      if (n == 0) {
        plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "")
        text(0, 0, "Click to add points.\nWhen there are at least 2 points the app will cluster automatically.", cex = 1.0)
      } else if (n == 1) {
        plot(rv$x, rv$y, xlim = xlim, ylim = ylim, xlab = "", ylab = "", pch = 19, col = "blue")
        text(0, 0, "Add at least one more point to enable clustering.", cex = 1.0)
      } else {
        # Try to cluster directly if there's no existing clustering (this is a fallback)
        data <- cbind(rv$x, rv$y)
        fit_try <- try(Mclust(data), silent = TRUE)
        if (!inherits(fit_try, "try-error") && !is.null(fit_try$classification)) {
          # Use the fit just for display, but don't treat as the canonical 'last cluster' here
          mclust2Dplot(data = data,
                       classification = fit_try$classification,
                       what = "classification",
                       main = FALSE,
                       xlim = xlim,
                       ylim = ylim)
          # record this as the initial clustering
          isolate({
            rv$fit <- fit_try
            rv$lastClusterN <- n
          })
        } else {
          plot(rv$x, rv$y, xlim = xlim, ylim = ylim, xlab = "", ylab = "", pch = 19, col = "blue")
          text(0, 0, "Unable to cluster these points. Add or move points.", cex = 1.0)
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)
