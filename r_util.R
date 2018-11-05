##This script contains several R functions I use frequently and would like quick access to.

load.packages <- function(packages)
{
  new.packages <- packages[!(packages %in% installed.packages())[,"Package"]]
  if(length(new.packages)) install.packages(new.packages)
  suppressMessages(library(packages))
}


load.defaults <- function()
{
  load.packages(c("readr", "magrittr", "tidyr", "dplyr"))

}

#Try using pacman package management