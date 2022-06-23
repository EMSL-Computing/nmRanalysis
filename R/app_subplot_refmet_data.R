#' Plot selected target (reference) metabolite data ROI's over brushed region of experimental data
#'
#' @param xpmt_data A dataframe with at least two columns. See details.
#' @param brushed_data plotly eventData corresponding to a brush event.
#' @param sample_to_plot A string specifying denoting which sample is featured in the main plot.
#' @param ROI_lines A list containing information necessary to create plotly shapes. See details.
#' @param ROI_annots A list containing information necessary to create plotly annotations. See details.
#' @param sourceid A string specifying the id of the generated plotly figure.
#'
#' @details This function generates a plotly figure specific to the experimental data at the brushed (selected) region of the main
#' plot. The first column of the dataframe required by the argument "xpmt_data" should contain the experimental PPM values.
#' Subsequent columns should contain the corresponding intensity values for each experimental sample.
#' The "ROI_lines" and "ROI_annots" lists should have as many elements as there are peak centers for the given target metabolite.
#' Each list element of "ROI_lines" and "ROI_annots" are lists containing several parameters that describe how interactive lines
#' and annotations should be drawn over the generated plotly figure. See ROI_line_gen() and ROI_annot_gen() for further details.
#'
#' @return A plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
subplot_refmet_data <- function(xpmt_data, brushed_data, sample_to_plot, ROI_lines, ROI_annots, sourceid){

  df_long <- xpmt_data %>%
    tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

  df_long <- df_long %>% dplyr::filter(.data$PPM >= min(brushed_data$x), .data$PPM <= max(brushed_data$x))
  df_long_selsamp <- df_long %>% dplyr::filter(.data$Sample == sample_to_plot)
  df_long_nonselsamp <- df_long %>% dplyr::filter(.data$Sample != sample_to_plot)

  # Include only the annotations within the selected range.
  tempidx <- lapply(ROI_lines, function(x){ifelse((x$x0 >= min(brushed_data$x) & x$x0 <= max(brushed_data$x)) |
                                                    (x$x1 >= min(brushed_data$x) & x$x0 <= max(brushed_data$x) |
                                                       ((x$x0+x$x1)/2 >= min(brushed_data$x) & (x$x0+x$x1)/2 <= max(brushed_data$x))),
                                                  TRUE, FALSE)})
  tempidx <- Reduce("c", tempidx)

  ROI_lines <- ROI_lines[tempidx]
  ROI_annots <- ROI_annots[tempidx]

  p <- plotly::plot_ly(source = sourceid) %>%
    plotly::config(displaylogo = FALSE,
                   modeBarButtons = list(list("zoom2d"), list("zoomIn2d"),
                                         list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                         list("resetScale2d"), list("toImage"))) %>%
    plotly::layout(xaxis = list(title     = "PPM",
                                autorange = "reversed"),
                   yaxis = list(title     = "Intensity"),
                   showlegend = TRUE,
                   dragmode = "zoom2d",
                   annotations = ROI_annots,
                   shapes      = ROI_lines) %>%
    plotly::add_trace(data = df_long_selsamp,
                      x=~.data$PPM, y=~.data$Intensity, type = "scatter", mode = "lines", name = sample_to_plot,
                      line = list(width = 1.3), hoverinfo = "text",
                      hovertext = paste0("Sample: ", sample_to_plot,
                                         "\nPPM: ", round(df_long_selsamp$PPM, 4),
                                         "\nIntensity: ", round(df_long_selsamp$Intensity, 4))) %>%
    plotly::add_trace(data = df_long_nonselsamp,
                      x=~.data$PPM, y=~.data$Intensity, type = "scatter", name = ~.data$Sample,
                      opacity = 0.3, mode = "lines", line = list(color = "#000000", width = 0.75),
                      hoverinfo = "text", hovertext = paste0("Sample: ", df_long_nonselsamp$Sample,
                                                             "\nPPM: ", round(df_long_nonselsamp$PPM, 4),
                                                             "\nIntensity: ", round(df_long_nonselsamp$Intensity, 4)),
                      showlegend = FALSE) %>%
    plotly::config(edits = list(annotationTail     = TRUE,
                                annotationText     = FALSE,
                                annotationPosition = FALSE,
                                shapePosition      = TRUE))

  return(list(plot   = p,
              ROIidx = tempidx))
}

