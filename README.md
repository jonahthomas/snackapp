# snackapp: Analysing SnackApp data in R

License: [MIT](https://github.com/jonahthomas/snackapp/blob/master/LICENSE.md)

This package has been designed to allow data from the Snacktivity SnackApp to be analysed within R. The package has mainly been designed for internal use by the Snacktivity team but has been made available on GitHub for potential use by future researchers. It has also been released to increase the repeatability of the data analysis conducted. 

## Installation

This package can be installted from GitHub with [devtools](https://github.com/hadley/devtools). 

```{r eval = FALSE}
library(devtools)
install_github("jonahthomas/snackapp")
library(snackapp)
```

## Basic set-up

As this package requires data collected from the SnackApp to be loaded into R, we describe how to set-up an R project to load data using functions default options. If you are a more advanced user of R, please feel free to bypass these steps and enter your own folder paths as desired. 

First, set up a new project within R. Once this project is created, create two folders within this project named "data" and "summary". Then, place all the files you wish to be analysed into the data folder. If the "csv" parameter is set to true, the output will be written to the "summary" folder. 

## In development... 

This package is still in development. Additional features are still being developed and will be added in time. The documentation for this package is still being developed and will continue to be improved over time. 

