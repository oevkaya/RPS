## RPS package

RPStest package is aimed to provide the functionality for the Research Program Strategy (RPS) as explained in the article **"Hypothesis-testing demands trustworthy data-a simulation approach to inferential statistics advocating the research program strategy"** by *Krefeld-Schwalb, Witte & Zenker (2018)*. The development version of RPS package is maintaining by Ozan Evkaya and Ekin Sibel Ceren. RPS is development version and feel free to contact for your questions and comments. 

With this package, the researchers can reproduce the results mentioned in the following shinny-app 

- https://antoniakrefeldschwalb.shinyapps.io/ResearchProgramStrategy/

## Installation 

To install the package from the github repo (Not in CRAN yet), devtools is required and the package can be installed by using following code:

```{r install}
# First you need the devtools package
# install.packages("devtools")
# library(devtools)

devtools::install_github("oevkaya/RPS")
```

After installing the package from github;

```{r setup}
library(RPS)
```

## Simple Usage

RPS package provides five easy-to-use functions and attached two more functions for visualization and the summarization of the outputs. As an example, **samplesH1** function requires four arguments; 

- Nsample

- alpha

- effectSize

- pow

```{r example}
h1 <- RPS::samplesH1(Nsample = 100, alpha = 0.05, effectSize = 0.1, pow = 0.95)

# From the output of samplesH1 function we have;
# Estimated sample size is 
h1$Nest

# Non-centrality-parameter of the t-distribution representing H1
h1$ncp

```

For more detailed calculations, interested reader is referred to the short vignette called **Intro_RPS**.

## Contact 

For any questions and feedback, please don't hesitate to contact us via following e-mail addresses:

* ozanevkaya@gmail.com

* esceren24@gmail.com

* a.krefeldschwalb@gmail.com

* frank.zenker@boun.edu.tr

## Citation 

- Krefeld-Schwalb, A., Witte Erich H., Zenker F. (2018). Hypothesis-Testing Demands Trustworthy Data - A Simulation Approach to Inferential Statistics Advocating the Research Program Strategy. Frontiers in Psychology, 9 (460). DOI=10.3389/fpsyg.2018.00460, ISSN= 1664-1078.

- Evkaya, O., Krefeld-Schwalb, A., Zenker, F. and Ceren, E.S., RPS: An R package for the functionality of the Research Program Strategy (RPS), GitHub, https://github.com/oevkaya/RPS




