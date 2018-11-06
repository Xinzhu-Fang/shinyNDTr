
# shinyNDTr - A web-based interface for neural decoding in R


## Getting started
* `install.packages(c("yaml","digest", "devtools", "curl"))`, which provide you with the basic tools for package development"
* For developlers:
   * Assume you working directory is a GitHub directory of all git repos
   1. `git clone https://github.com/emeyers/shinyNDTr.git`
   2. `git clone https://github.com/emeyers/NDTr.git`
   3. `cd shinyNDTr` 
   4. `devtools::install_local("../NDTr")`
* For users:
  1. do `devtools::install_github("emeyers/shinyNDTr")` to install shinyNDTr from GitHub.
  2. Because NDTr is not in CRAN yet, to install NDTr you could either do `devtools::install_github("emeyers/NDTr")`, which install NDTr from GitHub, or do `devtools::install_github(“r-lib/devtools”, ref = “7211ff8bff6d109fd1d9079080205306f4225ec8”)` so that the `Remotes` tag in `DESCRIPTION` is processed and the previous operation will be automatically done for you.
  3. If you want to use the data in NDTr, you have to `git clone https://github.com/emeyers/NDTr.git`
## Use the app
* `shiny::runApp('shinyNDTr')`

