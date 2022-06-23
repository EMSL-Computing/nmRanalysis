#' rDolphin: integration
#'
#' @param clean_fit 
#' @param Xdata 
#' @param Ydata 
#' @param buck_step 
#' @param interface 
#'
#' @details This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This function performs quantification through integration. 
#'
integration <- function(clean_fit, Xdata, Ydata, buck_step, interface='F'){
  
  #preallocation of results_to_save
  results_to_save = list(
    chemical_shift = NA,
    quantification = NA,
    signal_area_ratio = NA,
    fitting_error = NA,
    intensity = NA,
    half_bandwidth = NA
  )
  
  baseline = rep(0,length(Xdata))
  #preparation of baseline, if specified by the user
  if (clean_fit == 'N') baseline = seq(min(Ydata[1:5]), min(Ydata[(length(Xdata) - 4):length(Xdata)]), len =length(Xdata))
  baseline[which((baseline-Ydata)>0)]=Ydata[which((baseline-Ydata)>0)]
  #integration ad chechk that there are no negative values
  integrated_signal = Ydata - baseline
  integrated_signal[integrated_signal<0]=0
  #preparation of results_to_save
  results_to_save$quantification = sum(integrated_signal)*buck_step
  results_to_save$intensity = max(integrated_signal)
  
  cumulative_area = cumsum(integrated_signal) / sum(integrated_signal)
  
  if (all(is.na(cumulative_area))) {
    p1=1
    p2=length(cumulative_area)
  } else {
    p1 = max(1,which(cumulative_area< 0.05)[length(which(cumulative_area< 0.05))])
    p2 = min(which(cumulative_area > 0.95)[1],length(cumulative_area))
  }
  
  results_to_save$signal_area_ratio = tryCatch((sum(integrated_signal[p1:p2]) / sum(Ydata[p1:p2])) *
                                                 100,error = function(e) NaN, silent=T)
  
  
  results_to_save$half_bandwidth = NaN
  
  wer=which.min(abs(cumulative_area-0.5))
  if (cumulative_area[wer]>0.5) {
    wer=c(max(1,wer-1),wer)
  } else {
    wer=c(wer,min(wer+1,length(Xdata)))
  }
  wer2=(0.5-cumulative_area[wer[1]])/diff(cumulative_area[wer])
  if (wer[1]==wer[2]) wer2=0
  results_to_save$chemical_shift = Xdata[wer[1]]-wer2*diff(Xdata[wer])
  if(results_to_save$chemical_shift == Inf) results_to_save$chemical_shift=mean(Xdata)
  
  
  p=''
  if (interface=='T') {
    plotdata = data.frame(Xdata, signal = integrated_signal)
    plotdata2 = data.frame(Xdata, Ydata)
    plotdata3 = reshape2::melt(plotdata2, id = "Xdata")
    plotdata3$variable = rep('Original Spectrum', length(Ydata))
    plotdata4 = data.frame(Xdata, integrated_signal)
    plotdata5 = reshape2::melt(plotdata4, id = "Xdata")
    p=plotly::plot_ly(plotdata,x = ~Xdata, y = ~signal, type = 'scatter', color= 'Signal',mode = 'lines', fill = 'tozeroy') %>% 
      plotly::add_trace(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL) %>%
      plotly::layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'),
             yaxis = list(range=c(0,max(Ydata)),title = "Intensity (arbitrary unit)"))
  }
  plot_data=rbind(integrated_signal,baseline,integrated_signal+baseline,integrated_signal)
  
  integration_variables=list(results_to_save=results_to_save,p=p,plot_data=plot_data)
  return(integration_variables)
}