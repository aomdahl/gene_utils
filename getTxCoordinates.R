#!/usr/bin/env Rscript
#Packages and dependcies
require(Homo.sapiens)
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
if(!require("Homo.sapiens", quietyl = TRUE))
	BiocManager::install("Homo.sapiens")
if(!require("pacman", quietyl = TRUE))
	install.packages("pacman")
library(pacman)
p_load(optparse, magrittr, Homo.sapiens,biomaRt, dplyr)

option_list = list(
  make_option(c("-f", "--file"), type="character", default="~/OneDrive - Johns Hopkins/Research_Ashton_MacBook_Pro/rna_seq_impute/test_lakshmi.txt", 
              help="dataset file name", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output file name [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

library(Homo.sapiens)
test.file <- read.csv(opt$file, header = FALSE)[,1]
mt.genes <- sapply(test.file, function(x) grepl(pattern = "^MT-", x))
message("Removing ", sum(mt.genes), " mitochondrial genes")
test.file <- test.file[!mt.genes]
possible.entrants <- test.file[(test.file %in% biomaRt::keys(Homo.sapiens::Homo.sapiens, keytype = "SYMBOL"))]
message(length(possible.entrants), " of the remaining ", length(test.file), " genes can be queried")


get.all <- biomaRt::select(Homo.sapiens::Homo.sapiens, possible.entrants, c("TXCHROM","TXSTART","TXEND"), "SYMBOL") %>% 
  filter(nchar(as.character(TXCHROM)) < 6)  %>% group_by(SYMBOL) %>% 
  mutate("start"= min(TXSTART),"end" =max(TXEND), ) %>% ungroup() %>% 
  dplyr::select(SYMBOL, TXCHROM, start, end) %>% distinct()

write.table(get.all, file=opt$out,quote = FALSE,sep = " ", row.names = FALSE)

