# Necessary packages to load if you wanna use it separately
# library(ggplot2)
# library(ggpubr)
# library(ggtext)

# FIGURE 1: Distribution of t-values for given H0 (red) and H1 (blue).
figure1 <- function(sampH1) {
  Nsample <- sampH1$Nsample
  d <- sampH1$d
  H0 <- H1 <- NULL
  ## ggplot version
  Nsampledf <- data.frame(H0 = rt(Nsample, sampH1$Nest-1), H1 = sampH1$Tval)
  colors <- c("H0" = "red", "H1" = "blue")

  # For H0 (white)
  histH0 <- ggplot2::ggplot(Nsampledf, aes(x = H0)) +
    geom_histogram(fill = "red", stat = "bin", color = "black") +
    geom_histogram(aes(x = H1), fill = "blue", alpha = 0.4, stat = "bin",
                   color = "black") +
    xlim(c(-max(sampH1$Tval), max(sampH1$Tval))) +
    ylim(c(0, Nsample/2)) + xlab("") + ylab("Frequency") +
    # For H1 (grey)
    # geom_histogram(aes(x = H1), fill = "blue", alpha = 0.4, stat = "bin", bins = 10) +
    labs(title = paste0("Distribution of t-values for given <span style = 'color: red;'>H0</span> and <span style = 'color: blue;'>H1</span>."),
         subtitle = c(paste0("d = ",d, ",", "N = ",round(sampH1$Nest), ",", "ncp = ",round(sampH1$ncp,2))),
         caption = "RPS package") +
    ggpubr::theme_pubr() +
    theme(plot.title = ggtext::element_markdown())

  return(histH0)
}

# To use this function
# sampH1 <- samplesH1(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
# figure1(sampH1 = sampH1)

#######################################################
# FIGURE 2: Proportion of LR as a function of criteria.

figure2 <- function(sampH1) {

  alpha <- sampH1$alpha
  pow <- sampH1$pow

LRpropL1_df <- c(sum(sampH1$L1x/sampH1$L0x >= pow/alpha),
                 sum(sampH1$L1x/sampH1$L0x < pow/alpha & sampH1$L1x/sampH1$L0x >= 3),
                 sum(sampH1$L1x/sampH1$L0x < 3))

LRpropLplus0_df <- c(sum(sampH1$Lplus0x/sampH1$L0x >= pow/alpha),
                     sum(sampH1$Lplus0x/sampH1$L0x < pow/alpha & sampH1$Lplus0x/sampH1$L0x >= 3),
                     sum(sampH1$Lplus0x/sampH1$L0x < 3))

# create a dataset for plotting
names.arg <- c(rep(paste("LR >=", round(pow/alpha,2)) , 2),
               rep(paste(pow/alpha,">LR>=3") , 2) ,
               rep("3>LR" , 2))

legend <- rep(c("L(d=delta|x)/L(d=0|x)" , "L(d>delta|x)/L(d=0|x)") , 3)

value <- c(LRpropL1_df[1], LRpropLplus0_df[1], LRpropL1_df[2], LRpropLplus0_df[2],
           LRpropL1_df[3], LRpropLplus0_df[3])

LR_df <- data.frame(names.arg, legend, value)
# LR_df
# Stacked bar plot
ggplot2::ggplot(LR_df, aes(fill = legend, y=value, x = names.arg)) +
  geom_bar(position="dodge", stat="identity") + xlab("") + ylab("") +
  geom_text(aes(label = value), position=position_dodge(width=0.9)) +
  guides(fill = guide_legend(title="Criteria")) +
  labs(title = "Proportion of LR as a function of criteria",
       caption = "RPS package") +
  ggpubr::theme_pubr(legend = "bottom")

}

# To use this function
# sampH1 <- samplesH1(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
# figure2(sampH1 = sampH1)

############################################################
# FIGURE 3: Distribution of t-values for given H0 (red) and
# H1, adapted to d observed (blue)

figure3 <- function(sampH0, sampH1) {

  Nsample <- sampH1$Nsample
  d <- sampH1$d
  H0 <- H1 <- NULL
  # samp = sampH0, sampT = sampH1
  ## ggplot version
  Nsampledf <- data.frame(H0 = rt(Nsample, sampH0$Nest-1), H1 = sampH0$Tval)
  colors <- c("H0" = "red", "H1" = "blue")

  # For H0 (red)
  histH0_adap <- ggplot2::ggplot(Nsampledf, aes(x = H0)) +
    geom_histogram(fill = "red", stat = "bin", color = "black") +
    geom_histogram(aes(x = H1), fill = "blue", alpha = 0.4, stat = "bin",
                   color = "black") +
    xlim(c(-max(sampH1$Tval), max(sampH1$Tval))) +
    ylim(c(0, Nsample/2)) + xlab("") + ylab("Frequency") +
    # For H1 (grey)
    # geom_histogram(aes(x = H1), fill = "blue", alpha = 0.4, stat = "bin", bins = 10) +
    labs(title = paste0("Distribution of t-values for given <span style = 'color: red;'>H0</span> and <span style = 'color: blue;'>H1</span>,
                        adapted to d observed"),
         subtitle = c(paste0("True d = ",0, ",", "N = ",round(sampH0$Nest), ",", "ncp(emp) = ",round(sampH0$ncp,2))),
         caption = "RPS package") +
    ggpubr::theme_pubr() +
    theme(plot.title = ggtext::element_markdown())

  return(histH0_adap)
}

# To use the function
# sampH0 <- samplesH0(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
# sampH1 <- samplesH1(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
# figure3(sampH0 = sampH0, sampH1 = sampH1)

##########################################################
# FIGURE 4: Proportion of LR as a function of selected criteria.

figure4 <- function(sampH0) {

  alpha <- sampH0$alpha
  pow <- sampH0$pow
  ## create a dataset for plotting
  LRpropL1_df <- c(sum(sampH0$L1x/sampH0$L0x >= pow/alpha),
                   sum(sampH0$L1x/sampH0$L0x < pow/alpha & sampH0$L1x/sampH0$L0x >= 3),
                   sum(sampH0$L1x/sampH0$L0x < 3))

  LRpropLplus0_df <- c(sum(sampH0$Lplus0x/sampH0$L0x >= pow/alpha),
                       sum(sampH0$Lplus0x/sampH0$L0x < pow/alpha & sampH0$Lplus0x/sampH0$L0x >= 3),
                       sum(sampH0$Lplus0x/sampH0$L0x < 3))

  # create names
  names.arg <- c(rep(paste("LR >=", round(pow/alpha,2)) , 2),
                 rep(paste(pow/alpha,">LR>=3") , 2) ,
                 rep("3>LR" , 2))

  # create legend
  legend <- rep(c("L(d=delta|x)/L(d=0|x)" , "L(d>delta|x)/L(d=0|x)") , 3)

  value <- c(LRpropL1_df[1], LRpropLplus0_df[1], LRpropL1_df[2], LRpropLplus0_df[2],
             LRpropL1_df[3], LRpropLplus0_df[3])

  LR_df <- data.frame(names.arg, legend, value)
  # LR_df
  # Stacked bar plot
  ggplot2::ggplot(LR_df, aes(fill = legend, y=value, x = names.arg)) +
    geom_bar(position="dodge", stat="identity") + xlab("") + ylab("") +
    geom_text(aes(label = value), position=position_dodge(width=0.9)) +
    guides(fill = guide_legend(title="Criteria")) +
    labs(title = "Proportion of LR as a function of selected criteria",
         caption = "RPS package") +
    theme_pubr(legend = "bottom")

}

# To use this function
# sampH0 <- samplesH0(Nsample = 100, alpha = 0.05, effectSize = 0.2, pow = 0.95)
# figure4(sampH0 = sampH0)

############################################################
# FIGURE 5: Distributions of t-values for given H1(=t empirical) (blue) und H0 (red)

figure5 <- function(sampEmp) {

  Nsample <- sampEmp$Nsample
  N <- sampEmp$N
  Tval <- sampEmp$Tval
  H0 <- H1 <- NULL
  ## ggplot version
  Nsampledf <- data.frame(H0 = rt(Nsample, N-1), H1 = sampEmp$Tval)
  colors <- c("H0" = "red", "H1" = "blue")

  # For H0 (red)
  histH0_adap <- ggplot2::ggplot(Nsampledf, aes(x = H0)) +
    geom_histogram(fill = "red", stat = "bin", color = "black") +
    geom_histogram(aes(x = H1), fill = "blue", alpha = 0.4, stat = "bin",
                   color = "black") +
    xlim(c(-max(sampEmp$Tval), max(sampEmp$Tval))) +
    ylim(c(0, Nsample/2)) + xlab("") + ylab("Frequency") +
    # For H1 (blue)
    # geom_histogram(aes(x = H1), fill = "blue", alpha = 0.4, stat = "bin", bins = 10) +
    labs(title = paste0("Distribution of t-values for given <span style = 'color: blue;'>H1</span> (=t empirical) and <span style = 'color: red;'>H0</span>. "),
         subtitle = c(paste0("1-beta = ", round(sampEmp$pow,2), ",", "d = ", round(sampEmp$d,2), ",", "ncp = ",round(sampEmp$Temp,2))),
         caption = "RPS package") +
    ggpubr::theme_pubr() +
    theme(plot.title = ggtext::element_markdown())

  return(histH0_adap)
}

# To use this function
# sampEmp <- samplesEmp(Nsample = 100, N = 30, Temp = 3, alphaEmp = 0.05)
# figure5(sampEmp = sampEmp)
