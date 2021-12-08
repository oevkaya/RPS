#' @title Creating samples from H1, given alpha, effect size (d) and power
#'
#' @importFrom stats dt pt qnorm rnorm
#'
#' @param Nsample It can be fixed or given by the user
#' @param alpha level of significance (α = probability of type I error)
#' @param effectSize the hypothesized effect size
#' @param pow the desired power (1−β)
#'
#' @return Simulated likelihood ratios prior to data collection with
#' Tval: t value
#' p: Calculated PCDF values D|L(d=0|x) ?
#' Nest: estimated group size
#' ncp: non-centrality-parameter of the t-distribution representing H1
#' Nsample: given sample size value
#' d: given effectsize value
#'
#' @export
#'
#' @examples
#' \dontrun{
#' samplesH1(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
#' }
#'
samplesH1 <- function(Nsample = 100, alpha, effectSize, pow) {

  Nsampl <- Nsample
  d <- effectSize

  # For storing results
  # L10 <- Lplus00 <-  rep(NA, Nsampl)
  p <- L0x <- Lplus0x <- L0x <- L1x <- LEmpx <- ddeviate <- Tval <- rep(NA, Nsampl)

  ### Samples from H1, given alpha, d and power
  #Estimate Sample Size
  Nest <-  ((qnorm(1 - alpha) + qnorm(pow)) / (d/(sqrt(2))))^2
  df <-  2 * Nest -2

  #Non-centrality parameter
  ncp <- d * sqrt(Nest) / sqrt(2)

  #Draw Nsampl studies
  for (ss in 1:Nsampl){
    #Distribution of Treatment condition
    x1 <- rnorm(Nest, d, 1)
    meanx1 <-  mean(x1)

    #Distribution of Control condition
    x0 <- rnorm(Nest, 0, 1)
    meanx0 <- mean(x0)

    #Calculate t-values
    Tval[ss] <- (meanx1 - meanx0) / sqrt(2/Nest)

    #Calculate PCDF values D|L(d=0|x)
    p[ss] <- 1 - pt(Tval[ss], df)

    L0x[ss] <- dt(Tval[ss], df, 0)
    Lplus0x[ss] <- 1-pt(Tval[ss], df, ncp)

    L1x[ss] <- dt(Tval[ss], df, ncp)
    LEmpx[ss] <- dt(Tval[ss], df, Tval[ss])

    ddeviate[ss] <- LEmpx[ss]/L1x[ss]
  }

  samp_h1 = list(Tval = Tval, p = p, L1x = L1x, L0x = L0x, Lplus0x = Lplus0x,
              LEmpx = LEmpx, ddeviate = ddeviate, Nest = Nest, ncp = ncp,
              Nsample = Nsample, alpha=alpha, d = effectSize, pow = pow)

  return(samp_h1)
}
