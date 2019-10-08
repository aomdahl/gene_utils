suppressMessages(library(stringr))
suppressMessages(library(magrittr))
suppressMessages(library(readr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(Xmisc)) 
suppressMessages(library(rms)) 

parser <- ArgumentParser$new()
parser$add_description("Rscript for quickly generating an optimal histogram from data, specify the data and the column name")
parser$add_argument("--input", type = 'character', help = "Specify the path the input file")
parser$add_argument('--metric', help = "Specify the name of the condition we are scoring on, a string", type = "character", default = "beta")
parser$add_argument('--help',type='logical',action='store_true',help='Print the help page') 

parser$add_argument('--output', help = "specify the name of the output file.", type = "character", default = "quick_hist.png")
parser$helpme()
args <- parser$get_args()

#A function to plot the correlation bar plots

dat <- read_tsv(args$input) %>% rename("beta" = args$metric) 
print("Data read in.")
#Something from stack oveflow to get a good bin size
breaks <- pretty(dat$beta, n = nclass.FD(dat$beta), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(dat,aes(beta))+geom_histogram(binwidth=bwidth,fill="white",colour="black")

#plot = qplot(beta, data = dat, geom="histogram")
ggsave(file = args$output)

