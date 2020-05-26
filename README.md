# FARSFunctions

This is the second course project in the "Building an R Package" course in the "Mastering Software Development in R" specialization provided by Johns Hopkins University on Coursera. In this project, we will be using the code we documented in the first course project to build an R package with full documentation. 

## Installation Insctructions
 
 To install, make sure you have the devtools package installed and loaded. Then run the following command:

```{r installation}
install_github("dandewaters/FARSFunctions")
library(FARSFunctions)
```

## Vignettes

Read the intro vignette by building with vignettes = TRUE and running the following command:

```{r vignettes}
vignette("Introduction", package="FARSFunctions")
```

## Travis Badge
[![Build Status](https://travis-ci.com/dandewaters/FARSFunctions.svg?branch=master)](https://travis-ci.com/dandewaters/FARSFunctions)
