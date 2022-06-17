pacman::p_load(stringr, magrittr, readr, tidyr, dplyr, ggplot2, Xmisc, rms)
parser <- ArgumentParser$new()
parser$add_description("Rscript for quickly generating an optimal histogram from data, specify the data and the column name")
parser$add_argument("--input", type = 'character', help = "Specify the path the input file")
parser$add_argument('--metric', help = "Specify the name of the condition we are scoring on, a string", type = "character", default = "QZAR")
parser$add_argument('--help',type='logical',action='store_true',help='Print the help page') 
parser$add_argument('--log', type = 'logical', action = 'store_true', help = "plot log counts", default = FALSE)
parser$add_argument('--output', help = "specify the name of the output file.", type = "character", default = "quick_hist.png")
parser$add_argument("--abs", help = "convert the metric to absolute value", type = "logical", default = FALSE)
parser$add_argument("--plot_mean", help = "plot the mean on the histogram", type = "logical", default = FALSE) #TO implement
parser$add_argument("--set_xaxis", help = "Specify how long the x-axis should be, good for comparing plots", type ="numeric", default = 0)
parser$add_argument("--xlab_name", help = "Set the x axis name", type = "character", default = "")
parser$helpme()


args <- parser$get_args()

#A function to plot the correlation bar plots

dat <- read_tsv(args$input) 
if(args$metric == "NONE")
{ #We assume the data then has only 1 column
    names(dat) <- c("QZAR")
}else{
    dat <- dat %>% rename("QZAR" = args$metric)
} 
print("Data read in.")
if(args$abs)
{
    dat$QZAR <- abs(dat$QZAR)
}
#Something from stack oveflow to get a good bin size
breaks <- pretty(dat$QZAR, n = nclass.FD(dat$QZAR), min.n = 1)
bwidth <- breaks[2]-breaks[1]
if (!args$log) {
t <- ggplot(dat,aes(QZAR))+geom_histogram(binwidth=bwidth,fill="white",colour="black") + xlab(args$metric)
} else {
t <- ggplot(dat,aes(QZAR))+geom_histogram(binwidth=bwidth,fill="white",colour="black")  + scale_y_log10() + xlab(args$metric) + ylab("log10 Count")
}
if (args$set_xaxis != 0)
{
 t <- t + xlim(-1,args$set_xaxis)
}
if(args$xlab_name != "")
{   
    t <- t + xlab(args$xlab_name)
}
#plot = qplot(QZAR, data = dat, geom="histogram")
t
ggsave(file = args$output)

