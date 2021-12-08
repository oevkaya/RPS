#' @title Samples from H0, given alpha, and power and d assumed
#'
#' @importFrom stats sd
#'
#' @param Nsample It can be fixed or given by the user
#' @param alpha level of significance (α = probability of type I error)
#' @param effectSize the hypothesized effect size
#' @param pow the desired power (1−β)
#'
#' @return Simulated likelihood ratios with
#' p: Calculated PCDF values D|L(d=0|x) ?
#' Tval: t value
#' Nest: estimated group size
#' ncp: non-centrality-parameter of the t-distribution representing H1
#' demps: obtained empirical effectsize
#'
#' @export
#'
#' @examples
#' \dontrun{
#' samplesH0(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
#' }
#'
samplesH0 <- function(Nsample=100, alpha, effectSize, pow) {

  Nsampl <- Nsample
  d <- effectSize

  # For storing results
  p <- L0x <- Lplus0x <- L0x <- L1x <- Tval <- demp <- rep(NA, Nsampl)

  Nest <- ((qnorm(1- alpha) + qnorm(pow)) / (d/(sqrt(2))))^2
  df <- 2 * round(Nest) -2

  for (ss in 1:Nsampl){

    x0 <- rnorm(round(Nest),0, 1)
    x0b <- rnorm(round(Nest),0, 1)

    meanx0 <- mean(x0)
    meanx0b <- mean(x0b)

    # obtained empirical effectsize ?
    demp <- (mean(x0) - mean(x0b)) / (sqrt((sd(x0)^2 + sd(x0b)^2) / 2))
    ncp <- d * sqrt(round(Nest)) / sqrt(2)

    # t-value based on the difference of the means
    Tval[ss] <- (meanx0 - meanx0b) / sqrt(2/round(Nest))

    #Calculate PCDF values D|L(d=0|x)
    p[ss] <- 1 - pt(Tval[ss], df)
    L0x[ss] <- dt(Tval[ss], df)
    Lplus0x[ss] <- 1 - pt(Tval[ss], df, ncp)
    L1x[ss] <- dt(Tval[ss], df, ncp)
  }

  samp_h0 <- list(p = p, Tval = Tval, L0x = L0x, Lplus0x = Lplus0x,
                L1x = L1x, Nest = Nest, ncp = ncp, demp = demp,
                alpha = alpha, pow = pow)

  return(samp_h0)

}
