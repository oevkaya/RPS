#' @title Creating samples from H1, given alpha, effect size (d) and power
#'
#' @importFrom stats dt pt qnorm rnorm
#'
#' @param Nsample It can be fixed or given by the user
#' @param alpha level of significance (α = probability of type I error)
#' @param effectSize the hypothesized effect size
#' @param pow the desired power (1−β)
#' @param samp The output of samplesH1 function, it is necessary for Nest here
#'
#' @return Simulated likelihood ratios L1x, L0x and Lplus0x
#' @export
#'
#' @examples
#' \dontrun{
#' samples30(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95, samp = samplesH1)
#' }
#'
samples30 <- function(Nsample = 100, alpha, effectSize, pow, samp) {

  # samp <- samplesH1(Nsample, alpha, effectSize, pow)
  Nsampl <- Nsample

  #Estimate Sample Size via the result of samplesH1
  Nest <- samp$Nest *.5
  df <- 2 * Nest -2
  d <- effectSize

  p <- L0x <- Lplus0x <- L0x <- L1x <- LEmpx <- ddeviate <- Tval <- rep(NA, Nsampl)

  #Non-centrality parameter
  ncp <- d * sqrt(Nest) / sqrt(2)

  #Draw Nsampl studies
  for (ss in 1:Nsampl){
    #Distribution of Treatment condition
    x1 <- rnorm(Nest, d, 1)
    meanx1 <- mean(x1)

    #Distribution of Control condition
    x0 <- rnorm(Nest,0, 1)
    meanx0 <- mean(x0)

    #Calculate t-values
    Tval[ss] <- (meanx1 - meanx0) / sqrt(2/Nest)

    #Calculate PCDF values D|L(d=0|x)
    p[ss] <- 1-pt(Tval[ss], df)

    # The probability density analysis on the Student t-distribution
    L0x[ss] <- dt(Tval[ss], df, 0)
    # The probability cumulative density of the Student t-distribution
    Lplus0x[ss] <- 1-pt(Tval[ss], df, ncp)

    L1x[ss] <- dt(Tval[ss], df, ncp)
    LEmpx[ss] <- dt(Tval[ss], df, Tval[ss])

    ddeviate[ss] <- LEmpx[ss]/L1x[ss]
  }

  samp30 <- list(L1x = L1x, L0x = L0x, Lplus0x = Lplus0x, alpha = alpha, d = effectSize)

  return(samp30)

}
