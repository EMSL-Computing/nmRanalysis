#' Plot uploaded experimental data
#'
#' @param xpmt_data A dataframe with at least two columns. See details.
#' @param sourceid A string specifying the id of the generated plotly figure.
#' @param sample_to_plot A string specifying denoting which sample should be plotted (for the main plot).
#' @param brushed_data plotly eventData corresponding to a brush event. This is used to inform subplot generation.
#'
#' @details This function generates a plotly figure of the user-uploaded experimental data. The first column of the dataframe 
#' required by the argument "xpmt_data" should contain the experimental PPM values. Subsequent columns should contain the 
#' corresponding intensity values for each experimental sample. 
#' 
#' @return A plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 
plot_xpmt_data <- function(xpmt_data, sourceid, sample_to_plot = NULL, brushed_data = NULL){
  
  if(!is.null(sample_to_plot) & is.null(brushed_data)){
    
    xpmt_data_sample <- xpmt_data %>% dplyr::select(.data$PPM, .data[[sample_to_plot]])
    df_long <- xpmt_data_sample %>%
      tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity") 
    
    p <- plotly::plot_ly(source = sourceid) %>% 
      plotly::config(displaylogo = FALSE, 
                     modeBarButtons = list(list("select2d"), list("zoom2d"), list("zoomIn2d"),
                                           list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                           list("resetScale2d"), list("toImage"))) %>%
      plotly::layout(title = paste("Experimental Data:", sample_to_plot),
                     xaxis = list(title     = "PPM", 
                                  autorange = "reversed"),
                     yaxis = list(title     = "Intensity"),
                     dragmode = "zoom2d") %>%
      plotly::add_trace(data = df_long, x=~.data$PPM, y=~.data$Intensity, type = "scatter",
                        mode = "lines", line = list(width = 1), hoverinfo = "text", 
                        text = paste0("PPM: ", round(df_long$PPM, 4),
                                      "\nIntensity: ", round(df_long$Intensity, 4)))
  } 
  
  if(!is.null(brushed_data)){
    
    df_long <- xpmt_data %>%
      tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")
    
    df_long <- df_long %>% dplyr::filter(.data$PPM >= min(brushed_data$x), .data$PPM <= max(brushed_data$x))
    df_long_selsamp <- df_long %>% dplyr::filter(.data$Sample == sample_to_plot)
    df_long_nonselsamp <- df_long %>% dplyr::filter(.data$Sample != sample_to_plot)
    
    p <- plotly::plot_ly(source = sourceid) %>% 
      plotly::config(displaylogo = FALSE, 
                     modeBarButtons = list(list("zoom2d"), list("zoomIn2d"),
                                           list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                           list("resetScale2d"), list("toImage"))) %>%
      plotly::layout(xaxis = list(title     = "PPM", 
                                  autorange = "reversed"),
                     yaxis = list(title     = "Intensity"),
                     showlegend = TRUE,
                     dragmode = "zoom2d") %>%
      plotly::add_trace(data = df_long_selsamp,
                        x=~.data$PPM, y=~.data$Intensity, type = "scatter", mode = "lines", name = sample_to_plot,
                        line = list(width = 1.3), hoverinfo = "text", 
                        hovertext = paste0("Sample: ", sample_to_plot,
                                           "\nPPM: ", round(df_long_selsamp$PPM, 4),
                                           "\nIntensity: ", round(df_long_selsamp$Intensity, 4))) %>%
      plotly::add_trace(data = df_long_nonselsamp %>% dplyr::group_by(.data$Sample),
                        x=~.data$PPM, y=~.data$Intensity, type = "scatter", name = ~.data$Sample,
                        opacity = 0.3, mode = "lines", line = list(color = "#000000", width = 0.75), 
                        hoverinfo = "text", hovertext = paste0("Sample: ", df_long_nonselsamp$Sample,
                                                               "\nPPM: ", round(df_long_nonselsamp$PPM, 4),
                                                               "\nIntensity: ", round(df_long_nonselsamp$Intensity, 4)),
                        showlegend = FALSE)
  }
  
  return(p)
}