#Quickly compare lists of files and determine the degree of error. Is it a rounding error?
pacman::p_load(tidyr, dplyr, ggplot2, optparse, data.table)
option_list <- list(
  make_option(c("--file_list"), type = 'character', help = "A file containing a list of files to compare by row (ie. row 1: file1 file2 compares file1 to file 2)"),
  make_option(c("-r", "--row_names"), type = "logical", action="store_true", help = "Specify if the files have input headers or not.", default = FALSE)
)
args <- parse_args(OptionParser(option_list=option_list))
f <- fread(args$file_list,header = FALSE)

for(i in 1:nrow(f))
{
  print(paste0("Comparing files: ",unlist(f[i,1]), "    ", unlist(f[i,2])))
  ref <- fread(unlist(f[i,1]))
  verify <- fread(unlist(f[i,2]))
  if(args$row_names)
  {
    ref <- ref[,-1]
    verify <- verify[,-1]
  }

#Series of checkes
#dimensions are the same
if(nrow(ref) != nrow(verify))
{
  message("Number of rows differ")
  next
}
message("Number of rows match")
#dimensions are the same
if(ncol(ref) != ncol(verify))
{
  message("Number of columns differ")
  next
}
message("Number of columns match")


#Identical files
if(all(ref == verify))
{
  message("Files are identical")
  message("----------------------------------------")
  next
}

#within 10 dps
deepest_match = ""
digit_check = 0:15
for (d in digit_check) {
  ref.r <- round(ref,digits = d)
  verify.r <- round(verify, digits = d)
  if(all(ref.r == verify.r))
  {
    deepest_match=paste0("Files match out to ", d, " decimal points")
  }
}
message(deepest_match)

#Distribution of errors
tot.error <- abs(ref - verify)
#print(summary(tot.error))
message("Max error: ", max(tot.error))
message("----------------------------------------")
}
