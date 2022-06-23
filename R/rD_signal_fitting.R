#' rDolphin: signal_fitting
#'
#' @param parS 
#' @param Xdata 
#' @param multiplicities 
#' @param roof_effect 
#' @param freq 
#'
#' @details This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is the function
#' that fits the lineshape patterns according to the specified fitting parameters. Modifications have been made to the original function to allow for
#' the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
#' roof effect field) containing parameters necessary to fit this new pattern and new fitting equations that make use of these additional
#' fields.
#'
signal_fitting <- function(parS, Xdata, multiplicities, roof_effect, roof_effect2, freq){
  
  i = as.numeric(parS[seq(1, length(parS) - 5, 6)])           # intensity of each signal peak
  p = as.numeric(parS[seq(2, length(parS) - 4, 6)])           # chemical shift of each peak
  w = as.numeric(parS[seq(3, length(parS) - 3, 6)])*0.5/freq  # half bandwidth modified by field strength (freq)
  g = as.numeric(parS[seq(4, length(parS) - 2, 6)])           # gaussian parameter
  j = as.numeric(parS[seq(5, length(parS) - 1, 6)])/freq      # j coupling modified by field strength (freq)
  # 6/7/22 modification by Javier E. Flores so that the doublet of doublet pattern can be fitted. Note that
  # for doublet of doublets, there are two J-coupling values. j2 is representative of the second j-coupling value.
  j2 = as.numeric(parS[seq(6, length(parS) - 0, 6)])/freq     # second j coupling (for dd only) modified by field strength (freq)
  
  signals_parameters=rbind(i,p,w,g,j,j2)
  fitted_signals = matrix(0, dim(signals_parameters)[2], length(Xdata))
  
  # multiplicities = as.numeric(parS[seq(6, length(parS) - 1, 7)])
  # roof_effect = as.numeric(parS[seq(7, length(parS) - 0, 7)])
  NumSignals = length(parS) / 6
  
  
  for (s in seq_along(multiplicities)) {
    if (roof_effect[s] > 0) {
      # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      if (multiplicities[s] %in% c("1", "s"))   {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("2", "d")) {
        fitted_signals[s, ] = peakpvoigt(c(
          signals_parameters[1, s]/(1 - roof_effect[s]),
          (signals_parameters[2, s] - signals_parameters[5, s]/2),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata) + peakpvoigt(
          c(
            signals_parameters[1, s],
            (signals_parameters[2, s] + signals_parameters[5, s]/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("3", "t")) {
        y=1/(2 + roof_effect[s])
        x= 1-y
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s] *x,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] *y,
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("4", "q")) {
        #     fitted_signals[s, ] = peakpvoigt(
        #       c(
        #         signals_parameters[1, s] / 3,
        #         (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #         signals_parameters[3, s],
        #         signals_parameters[4, s]
        #       ),
        #       Xdata
        #     ) + peakpvoigt(c(
        #       signals_parameters[1, s],
        #       (signals_parameters[2, s] - signals_parameters[5, s]),
        #       signals_parameters[3, s],
        #       signals_parameters[4, s]
        #     ),
        #     Xdata) + peakpvoigt(c(
        #       signals_parameters[1, s],
        #       (signals_parameters[2, s] + signals_parameters[5, s]),
        #       signals_parameters[3, s],
        #       signals_parameters[4, s]
        #     ),
        #     Xdata) + peakpvoigt(
        #       c(
        #         signals_parameters[1, s] / 3,
        #         (signals_parameters[2, s] + 3 * signals_parameters[5, s]),
        #         signals_parameters[3, s],
        #         signals_parameters[4, s]
        #       ),
        #       Xdata
        #     )
        # 6/6/22 modification by Javier E. Flores to accommodate doublet of doublet fitting
      } else if (multiplicities[s] %in% c("dd")) {
        if(roof_effect2[s] > 0){
          
          fitted_signals[s, ] = peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect[s])/(1 - roof_effect2[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(
            c(
              signals_parameters[1, s]/(1 - roof_effect2[s]),
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          )
        } else if(roof_effect2[s] < 0){
          
          fitted_signals[s, ] = peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect[s]) * (1-roof_effect2[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s] * (1-roof_effect2[s]),
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          )
          
        } else if(roof_effect2[s] == 0){
          fitted_signals[s, ] = peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          )
        }
        
      }
    } else if (roof_effect[s] == 0) {
      # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      if (multiplicities[s] %in% c("0")) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("1", "s")) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("2", "d")) {
        fitted_signals[s, ] = peakpvoigt(c(
          signals_parameters[1, s],
          (signals_parameters[2, s] - signals_parameters[5, s]/2),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata) + peakpvoigt(c(
          signals_parameters[1, s],
          (signals_parameters[2, s] + signals_parameters[5, s]/2),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata)
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("3", "t")) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s] / 2,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] / 2,
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("4", "q")) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s] / 3,
            (signals_parameters[2, s] - 3 * signals_parameters[5, s]/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(c(
          signals_parameters[1, s] ,
          (signals_parameters[2, s] - signals_parameters[5, s]/2) ,
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata) + peakpvoigt(c(
          signals_parameters[1, s],
          (signals_parameters[2, s] + signals_parameters[5, s]/2),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata) + peakpvoigt(
          c(
            signals_parameters[1, s] / 3,
            (signals_parameters[2, s] + 3 * signals_parameters[5, s]/2) ,
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] %in% c("dd")) {
        if(roof_effect2[s] > 0){
          
          fitted_signals[s, ] = peakpvoigt(c(
            signals_parameters[1, s]/(1 - roof_effect2[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(
            c(
              signals_parameters[1, s]/(1 - roof_effect2[s]),
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          )
        } else if (roof_effect2[s] < 0){
          
          fitted_signals[s, ] = peakpvoigt(c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s] * (1 - roof_effect2[s]),
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s] * (1 - roof_effect2[s]),
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          )
        } else if (roof_effect2[s] == 0){
          fitted_signals[s, ] = peakpvoigt(c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          )
        }
      }
    } else if (roof_effect[s] < 0) {
      # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      if (multiplicities[s] %in% c("1", "s")) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("2", "d")) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - signals_parameters[5, s]/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(c(
          signals_parameters[1, s] *
            (1 - roof_effect[s]) ,
          (signals_parameters[2, s] + signals_parameters[5, s]/2),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata)
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("3", "t")) {
        y=1/(2 + roof_effect[s])
        x= 1-y
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s]*x,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s]*y,
            (signals_parameters[2, s] + signals_parameters[5, s]) ,
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
        # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
      } else if (multiplicities[s] %in% c("4", "q")) {
        # fitted_signals[s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3 ,
        #     (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   Xdata
        # ) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] - signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # Xdata) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] + signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # Xdata) + peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] + 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   Xdata
        # )
      } else if (multiplicities[s] %in% c("dd")) {
        
        if(roof_effect2[s] > 0){
          fitted_signals[s, ] = peakpvoigt(
            c(
              signals_parameters[1, s]/(1- roof_effect2[s]),
              (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(c(
            signals_parameters[1, s] *
              (1 - roof_effect[s])/(1- roof_effect2[s]) ,
            (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s] *
              (1 - roof_effect[s]) ,
            (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata)
        } else if (roof_effect2[s] < 0){
          fitted_signals[s, ] = peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s] * (1 - roof_effect2[s]),
              (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(c(
            signals_parameters[1, s] *
              (1 - roof_effect[s]) ,
            (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s] *
              (1 - roof_effect[s]) * (1 - roof_effect2[s]) ,
            (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata)
        } else if (roof_effect2[s] == 0){
          fitted_signals[s, ] = peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2 - signals_parameters[6, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] - (signals_parameters[5, s] - signals_parameters[6, s])/2),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            Xdata
          ) + peakpvoigt(c(
            signals_parameters[1, s] *
              (1 - roof_effect[s]) ,
            (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata) + peakpvoigt(c(
            signals_parameters[1, s] *
              (1 - roof_effect[s]) ,
            (signals_parameters[2, s] + (signals_parameters[5, s] - signals_parameters[6, s])/2 + signals_parameters[6, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata)
        }
        
      }
    }
  }
  
  
  return(fitted_signals)
}