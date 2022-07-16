## ResProStr package

ResProStr package is aimed to provide the functionality for the Research Program Strategy (ResProStr) as explained in the article **"Hypothesis-testing demands trustworthy data-a simulation approach to inferential statistics advocating the research program strategy"** by *Krefeld-Schwalb, Witte & Zenker (2018)*. The development version of ResProStr package is maintaining by Ozan Evkaya - Ekin Sibel Ceren and feel free to contact for your questions and comments. 

With this package, the researchers can reproduce the results mentioned in the following shinny-app 

- https://antoniakrefeldschwalb.shinyapps.io/ResearchProgramStrategy/

## Installation 

To install the package from the github repo (Not in CRAN yet), devtools is required and the package can be installed by using following code:

```{r install}
# install.packages("devtools")
# library(devtools)
```

```{r install}
devtools::install_github("oevkaya/ResProStr")
```

After installing the package from github;

```{r setup}
library(ResProStr)
```

## Simple Usage

ResProStr package provides five easy-to-use functions and attached two more functions for visualization and the summary of the outputs. As an example, **samplesH1** function requires four arguments; 

- Nsample

- alpha

- effectSize

- pow

```{r example}
h1 <- ResProStr::samplesH1(Nsample = 100, alpha = 0.05, effectSize = 0.1, pow = 0.95)
```

From the output of samplesH1 function, estimated sample size is; 

```{r sample}
h1$Nest
```

Non-centrality-parameter of the t-distribution representing H1

```{r ncpar}
h1$ncp
```

For more detailed calculations, interested reader is referred to the short vignette called **Intro_ResProStr**.

## Contact 

For any questions and feedback, please don't hesitate to contact us via following e-mail addresses:

* ozanevkaya@gmail.com

* esceren24@gmail.com

* a.krefeldschwalb@gmail.com

* frank.zenker@boun.edu.tr

## Citation 

- Krefeld-Schwalb, A., Witte Erich H., Zenker F. (2018). Hypothesis-Testing Demands Trustworthy Data - A Simulation Approach to Inferential Statistics Advocating the Research Program Strategy. Frontiers in Psychology, 9 (460). DOI=10.3389/fpsyg.2018.00460, ISSN= 1664-1078.

- Evkaya, O., Krefeld-Schwalb, A., Zenker, F. and Ceren, E.S., ResProStr: An R package for the functionality of the Research Program Strategy (ResProStr), GitHub, https://github.com/oevkaya/RPS




