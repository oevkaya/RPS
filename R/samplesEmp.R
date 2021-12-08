#' @title Simulate likelihood ratios post-hoc
#'
#' @importFrom stats dt pt pnorm rnorm qnorm
#'
#' @param Nsample It can be fixed or given by the user
#' @param N The sample size N per group
#' @param Temp Empirical t-value
#' @param alphaEmp Empirical level of significance
#'
#' @return Simulate likelihood ratios with
#' demp: obtained empirical effectsize
#' pow: the desired power (1−β)
#' Tval: t value
#' p: Calculated PCDF values D|L(d=0|x) ?
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' samplesEmp(Nsample = 100, N = 30, Temp = 3, alphaEmp = 0.05)
#' }
#'
samplesEmp <- function(Nsample = 100, N, Temp, alphaEmp) {
  # sample size
  Nsampl <-  Nsample

  # For storing results
  p <-  L0x <-  Lplus0x <-  L0x <-  L1x <- Tval <- rep(NA, Nsampl)

  # Empirical effectsize based on given t-value
  demp <- sqrt(2/N) * Temp
  # The post-hoc estimate of the power
  pow <- pnorm(sqrt(N) * demp/sqrt(2) - qnorm(1 - alphaEmp))
  df <- 2 * N -2

  for (ss in 1:Nsampl){
    #Distribution of Treatment condition
    x1 <- rnorm(N,demp, 1)
    meanx1 <- mean(x1)

    #Distribution of Control condition
    x0 <- rnorm(N,0, 1)
    meanx0 <- mean(x0)

    #Calculate t-values
    Tval[ss] <- (meanx1-meanx0) / sqrt(2/N)

    #Calculate PCDF values D|L(d=0|x)
    p[ss] <- 1-pt(Tval[ss], df)

    L0x[ss] <- dt(Tval[ss], df, 0)
    Lplus0x[ss] <- 1-pt(Tval[ss], df, Temp)

    L1x[ss] <- dt(Tval[ss], df, Temp)

  }

  samp_emp <- list(d = demp, pow = pow, Tval = Tval, p = p,
              L1x = L1x, L0x = L0x, Lplus0x = Lplus0x,
              Nsample = Nsample, N = N, Tval = Tval,
              Temp = Temp, pow = pow, alphaEmp = alphaEmp)

  return(samp_emp)
}
