
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ```{r, include = FALSE}
# knitr::opts_chunk$set(
#   collapse = TRUE, comment = "#>",
#   fig.path = "man/figures/README-", out.width = "100%"
# )
# ```

## RPS package

RPStest package is aimed to provide the functionality for the Research Program Strategy (RPS) as explained in the article **"Hypothesis-testing demands trustworthy data-a simulation approach to inferential statistics advocating the research program strategy"** by *Krefeld-Schwalb, Witte & Zenker (2018)*. 

The idea for RPS originates with Erich H. Witte. Frank Zenker and Erich H. Witte jointly developed its presentation. Antonio Krefeld-Schwalb programed and ran the simulations. Erich H. Witte wrote the first draft of the manuscript; all authors edited it and approved the final version. The development version of RPS package is maintaining by Ozan Evkaya and Ekin Sibel Ceren. RPS is development version and feel free to contact for your questions and comments. 

With this package, the researchers can easily reproduce the results mentioned in the Shinny-app 

- https://antoniakrefeldschwalb.shinyapps.io/ResearchProgramStrategy/

## Installation 

To install the package from the github repo, devtools is required and the package can be installed by using following code:

```{r install}
# First you need the devtools package
# install.packages("devtools")
# library(devtools)

# devtools::install_github("oevkaya/RPS")
```

After installing the package from github repo, call it;

```{r setup}
library(RPS)
```

For more detailed calculations, interested reader is guided to look at the short vignette for the usage.

## Citation 

If you use RPS package, please cite it as;

@misc{RPS, 
  author = {Evkaya, O., Krefeldschwalb, A., Zenker, F. and Ceren, E.S.},
  title = {RPS: An R package for the functionality of the Research Program Strategy (RPS)
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\url{https://github.com/oevkaya/RPS}}
}

## Contact 

For any questions and feedback, please dont hesitate to contact us via following e-mail adresses:

* ozanevkaya@gmail.com

* a.krefeldschwalb@gmail.com

* frank.zenker@boun.edu.tr

* esceren24@gmail.com






