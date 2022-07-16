# Necessary packages
# library(gt)
# library(magrittr)

# Table 1
# Proportion of correct positive results.
  # samp = samp_h1
  # samp30 = samples30(Nsample = 100, alpha = alpha, effectSize = d, pow = pow, samp = samp_h1)

Table1 <- function(sampH1, samp30){

  d <- samp30$d
  alpha <- sampH1$alpha

  df = 2 * sampH1$Nest-2
  pow = pnorm(sqrt(sampH1$Nest * 1.5) * d / sqrt(2) - qnorm(1 - alpha))

  diff = mean(sampH1$Lplus0x/sampH1$L0x >=pow/alpha) - round(sum(sampH1$ddeviate[sampH1$L1x/sampH1$L0x >=pow/alpha] < dt( qt(.5, df, sampH1$ncp),df,sampH1$ncp)/dt( qt(.95, df,sampH1$ncp),df,sampH1$ncp))/100,2)

  LR = c(mean(sampH1$Lplus0x / sampH1$L0x >= pow/alpha),
         mean(sampH1$L1x / sampH1$L0x >= pow/alpha),
         round(sum(sampH1$ddeviate[sampH1$L1x / sampH1$L0x >=pow/alpha] < dt( qt(.5, df,sampH1$ncp),df,sampH1$ncp)/dt( qt(.95, df,sampH1$ncp),df,sampH1$ncp))/100,2),
         diff,
         round(mean((log(sampH1$L1x/sampH1$L0x) + log(samp30$L1x/samp30$L0x)) >= log(pow/(1-pow))),2)
  )


  LR = data.frame(cbind(c("4. Substantial Falsification",
                          "5. Preliminary Verification",
                          "6. Substantial Verification",
                          "False Negatives: Substantial Falsification - Substantial Verification",
                          paste("Substantial Verification if N'= N + N/2",
                          round(sampH1$Nest*.5),", 1-beta=",round(pow,2))), LR))

  colnames(LR) = c("Step in ResProStr","Proportion")
  Table <- LR %>% gt::gt(caption = "Proportion of correct positive results
                      (prior to data collection)")
  return(Table)
}

# To use the function
# Table1(sampH1 = sampH1, samp30 = sample30)

#############################################
# Table 2
# Proportion of false positive results.

Table2 <- function(sampEmp0, sampEmp){

  alphaemp <- sampEmp$alphaEmp

  LR = c(mean(sampEmp0$Lplus0x/sampEmp0$L0x >= sampEmp$pow/alphaemp),
         mean(sampEmp0$L1x/sampEmp0$L0x >= sampEmp$pow/alphaemp))

  LR = data.frame(cbind(c("4. Substantial Falsification",
                          "5. Preliminary Verification"), LR))

  colnames(LR) = c("Step in ResProStr", "Proportion")

  Table <- LR %>%
  gt::gt(caption = " Proportion of false positive results (prior to data collection)")
  return(Table)

}

# To use the function
# Table2(sampEmp0 = sampleEmp0, samplesEmp = sampleEmp)

##########################################################
# Table 3
# Proportion of correct positive results

Table3 <- function(sampEmp){

  alphaemp <- sampEmp$alphaEmp
# Substantial falsification & Preliminary Verification
LR = c(mean(sampEmp$Lplus0x/sampEmp$L0x >= sampEmp$pow / alphaemp),
      mean(sampEmp$L1x/sampEmp$L0x >= sampEmp$pow / alphaemp))

LR = data.frame(cbind(c("4. Substantial Falsification",
                        "5. Preliminary Verification"), LR))

    colnames(LR) = c("Step in ResProStr", "Proportion")

    Table <- LR %>% gt::gt(caption = " Proportion of correct positive results (post-hoc)")
    return(Table)

}

# To use the function
# Table3(sampEmp = sampleEmp)
######################################################
# Table ?

# samp <-  samp_h1
# samp0 <-  samplesH0(Nsample=100, alpha, effectSize, pow)
#
# LR = data.frame(cbind(min(samp$L1x/samp$L0x),
#                 quantile(samp0$L1x/samp0$L0x, .99)))
#
# colnames(LR) = c("min(L(d|x)/L(d=0|x)) if H1 is true",
#                        "max(L(d|x)/L(d=0|x)) if H0 is true")
# pander(LR)
###################################################################
# Table 4
# Proportion of false positive results.?
# samp = samp0

Table4 <- function(sampH0){

  pow <- sampH0$pow
  alpha <- sampH0$alpha

LR = c(mean(sampH0$Lplus0x/sampH0$L0x >= pow/alpha),
       mean(sampH0$L1x/sampH0$L0x >= pow/alpha))

LR = data.frame(cbind(c("4. Substantial Falsification",
                        "5. Preliminary Verification") ,LR))

colnames(LR) = c("Step in ResProStr", "Proportion")

Table <- LR %>%
  gt::gt(caption = " Proportion of false positive results (post-hoc)")

return(Table)

}

# To use the function
# Table4(sampH0 = sampH0)

###############################


