#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#arguments: 1 is the file to sort, 2 is the column by which to sort. Assumes descending
library(tidyr)
library(dplyr)
library(data.table)
library(readr)


f <- fread(args[1])
#col_name <- names(f)[as.numeric(args[2])]
col <- as.numeric(args[2])
ordered <- order(abs(f[,..col]), decreasing = TRUE)
f <- f[ordered,]
write_tsv(f, file = args[3], col_names = FALSE)
