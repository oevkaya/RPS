#' @title Samples from different H relative to H1, given alpha, and Nest d
#'
#' @importFrom stats dt pt qnorm rnorm
#'
#' @param Nsample It can be fixed or given by the user
#' @param alpha level of significance (α = probability of type I error)
#' @param effectSize the hypothesized effect size
#' @param pow the desired power (1−β)
#' @param samp The output of samplesH1 function
#'
#' @return The proportions of simulated t-values for the RSP methodology
#' Here, the Wald-criterion (1−β)/α is applied for interpreting the obtained
#' likelihood ratios
#' @export
#'
#' @examples
#' \dontrun{
#' sumLR(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95, samp = samplesH1)
#' }
#'
sumLR <- function(Nsample, alpha, effectSize, pow, samp) {

  # For storing results
  p <- L0x <-  Lplus0x <-  L0x <-  L1x <-  LEmpx <-  ddeviate <-  Tval <-  rep(NA, Nsample)

  # samp <- samplesH1(Nsample, alpha, effectSize, pow)

  d <- effectSize
  # Creation of different effect size values with given d
  del <- c(0, .25 * d, .5 * d, .75 * d,
          min(1, 1.25 * d), min(1, 1.5 * d), min(1, 1.75 * d), min(1, 2 * d), 1)

  probLR <- matrix(NA, nrow= length(del), ncol = 2)

  #Estimate Sample Size
  Nest <- ((qnorm(1- alpha) + qnorm(pow)) / (d/(sqrt(2)))) ^ 2
  df <- 2 * Nest -2

  #Non-centrality parameter
  for (dd in 1:length(del)) {

    ncp <- del[dd] * sqrt(Nest)/sqrt(2)
    #Draw Nsampl studies
    for (ss in 1:Nsample){
      #Distribution of Treatment condition
      x1 <- rnorm(Nest,del[dd], 1)
      meanx1 <- mean(x1)

      #Distribution of Control condition
      x0 <- rnorm(Nest,0, 1)
      meanx0 <- mean(x0)

      #Calculate t-values
      Tval[ss] <- (meanx1-meanx0)/sqrt(2/Nest)

      #Calculate PCDF values D|L(d=0|x)
      p[ss] = 1 - pt(Tval[ss], df)

      L0x[ss] <- dt(Tval[ss], df, 0)
      Lplus0x[ss] <- 1 - pt(Tval[ss], df, ncp)

      L1x[ss] <- dt(Tval[ss], df, ncp)
      LEmpx[ss] <- dt(Tval[ss], df, Tval[ss])

      ddeviate[ss] <- LEmpx[ss] / L1x[ss]
    }

    # Preliminary and Substantial Falsification
    probLR[dd, 1:2] <- c(mean(Lplus0x/L0x >= pow / alpha),
                       mean(L1x/L0x >= pow / alpha))
}

  probLR <- rbind(probLR[1:4,],
                  c(mean(samp$Lplus0x / samp$L0x >= pow / alpha),
                    mean(samp$L1x / samp$L0x >= pow / alpha)), probLR[5:9,])

  pLR <- list(probLR = probLR, d = c(del[1:4], d, del[5:9]))

  return(pLR)

}
