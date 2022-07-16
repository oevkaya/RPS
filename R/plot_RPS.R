#' Creating of whole plots by selection
#'
#' @importFrom stats rt
#' @import ggplot2
#' @import ggpubr
#' @import ggtext
#' @import patchwork
#'
#' @param sampH0 The result of samplesH0 function
#' @param sampH1 The result of samplesH1 function
#' @param sampEmp The result of samplesEmp function
#' @param select The selection of plot options
#'
#' @return Plot summary of ResProStr. Based on the selection, different plots
#' are generated;
#' select = 1: Distribution of t-values for given H0 and H1
#' select = 2: Proportion of LR as a function of criteria
#' select = 3: Distribution of t-values for given H0 and H1 adapted to d observed
#' select = 4: Proportion of LR as a function of selected criteria.
#' select = 5: Distributions of t-values for given H1(=t empirical) und H0
#' select = 6: Multiple plots for the case of the likelihood ratios prior to data collection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_RPS(sampH0 = sampH0, sampH1 = sampH1, sampEmp = sampEmp,
#' select = 6)
#' }

plot_RPS <- function(sampH0 = sampH0, sampH1 = sampH1, sampEmp = sampEmp,
                     select = 6) {
 if (is.null(select) == TRUE) {
   write("Please make a selection for model diagnostics")
 }
 # For Figure 1
  if (select == 1) {
    Plot <- figure1(sampH1 = sampH1) # Check
  }
  # For Figure 2
  if (select == 2) {
    Plot <- figure2(sampH1 = sampH1) # Check
  }
  # For Figure 3
  if (select == 3) {
    Plot <- figure3(sampH0 = sampH0, sampH1 = sampH1) # Check
  }
  # For Figure 4
  if (select == 4) {
    Plot <- figure4(sampH0 = sampH0) # Check
  }
  # For Figure 5
  if (select == 5) {
    Plot <- figure5(sampEmp = sampEmp) # Check
  }

  # For whole summary: Simulate likelihood ratios prior to data collection
  if (select == 6) {
    #Plot1 <- figure1(sampH1 = sampH1)
    #Plot2 <- figure2(sampH1 = sampH1)
    #Plot3 <- figure3(sampH0 = sampH0, sampH1 = sampH1)
    #Plot4 <- figure4(sampH0 = sampH0)

    Plot <- (figure1(sampH1 = sampH1) + figure2(sampH1 = sampH1)) /
                (figure3(sampH0 = sampH0, sampH1 = sampH1) + figure4(sampH0 = sampH0))
  }
  return(Plot)
}
