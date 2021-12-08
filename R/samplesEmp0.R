#' @title Simulate likelihood ratios post-hoc under H0 ?
#'
#' @importFrom stats dt pt pnorm rnorm qnorm
#'
#' @param Nsample It can be fixed or given by the user
#' @param N The sample size N per group
#' @param Temp Empirical t-value
#' @param alphaEmp Empirical level of significance
#'
#' @return Simulate likelihood ratios with
#' p: Calculated PCDF values D|L(d=0|x) ?
#' Tval: t value
#' ncp: non-centrality-parameter of the t-distribution
#' demp: obtained empirical effectsize
#'
#' @export
#'
#' @examples
#' \dontrun{
#' samplesEmp0(Nsample = 100, N = 30, Temp = 3, alphaEmp = 0.05)
#' }
#'
samplesEmp0 <- function(Nsample, N, Temp, alphaEmp) {

  # sampEmp <- samplesEmp(Nsample, N, Temp, alphaEmp, pow)
  Nsampl <- Nsample

  # For storing results
  p <- L0x <- Lplus0x <- L0x <- L1x <- demp <- Tval <- rep(NA, Nsampl)

  df <- 2 * round(N) - 2

  for (ss in 1:Nsampl){

    x0 <- rnorm(round(N), 0, 1)
    x0b <- rnorm(round(N), 0, 1)

    meanx0 <- mean(x0)
    meanx0b <- mean(x0b)

    demp <- (mean(x0) - mean(x0b)) / (sqrt((sd(x0)^2 + sd(x0b)^2)/2))
    ncp <- demp * sqrt(round(N))/sqrt(2)

    Tval[ss] <- (meanx0-meanx0b) / sqrt(2/round(N))

    p[ss] <- 1-pt(Tval[ss], df)
    L0x[ss] <- dt(Tval[ss], df)
    Lplus0x[ss] <- 1-pt(Tval[ss], df, ncp)
    L1x[ss] <- dt(Tval[ss], df, ncp)
  }

  samp_emp0 <- list(p=p, Tval = Tval, L0x = L0x, Lplus0x = Lplus0x,
                L1x = L1x, ncp = ncp, demp = demp,
                Nsample = Nsample, N = N, Tval = Tval)

  return(samp_emp0)
}
