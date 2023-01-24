pacman::p_load(data.table, tidyr, dplyr, readr, optparse, stringr, tools)
###################
#Script to check the md5sums of a list of files by calculating them on the fly and comparing them to a reference
#The easiest use is to pass in a 

###################

checkSum <- function(full_path, manifest, name_col = 5, key_col = 8)
{
  dname <- basename(full_path)
  mdsum <- unlist(manifest[which(manifest[,..name_col] == dname), ..key_col])
  if(length(mdsum) == 0)
  {
    #message(paste0("File ", full_path, " was not found in the reference and will not be checked."))
    return(paste0("File ", full_path, " was not found in the reference and will not be checked."))
  }
  check <- unlist(tools::md5sum(paste0(full_path)))
  if(check != mdsum)
  {
    
    return((paste0("Error in downloading: ", dname, " please try again!")))
  } 
  else{
    #print(paste0("Succesfully downloaded: ", dname, "!"))
    return(1)
  }
}

option_list <- list(
  make_option("--md5_key", type = 'character', help = "File containing the file names (column 1) and the md5sums (column 2", 
              default = "/scratch16/abattle4/lab_data/UKBB/GWAS_Neale/heritability_estimates/manifest_jan_2023.csv"),
  make_option("--dir", type = "character", help = "Path to files we want", default ="/scratch16/abattle4/lab_data/UKBB/GWAS_Neale/highly_heritable_traits_2/" ),
  make_option("--pattern", type = "character", help = "wildcard to get the files you want", default = ".*.bgz"),
  make_option("--check_files", type = "character", help = "path to list of files with full paths to check", default = ""),
  make_option("--key_col", type = "integer", default =9, help = "Index of column containing true md5sums (1-based index)"),
  make_option("--name_col", type = "integer", default =5, help = "Index of column containing reference file names (1-based index)"),
  make_option('--help',type='logical',action='store_true',help='Print the help page')
)
args <- parse_args(OptionParser(option_list=option_list))

    if(args$check_files == "")
    {
      check_files <- paste0(args$dir, list.files(args$dir, pattern = args$pattern))
    }else
    {
      check_files <- unlist(fread(check_files)[,1])
    }
    
    #check_files <- list.files("/scratch16/abattle4/lab_data/UKBB/GWAS_Neale/highly_heritable_traits_2/", pattern = ".*.bgz") #get teh full paths 
    manifest <- fread(args$md5_key)
    #hits <- sapply(check_files[1:5], function(x) checkSum(x,"/scratch16/abattle4/lab_data/UKBB/GWAS_Neale/highly_heritable_traits_2/", manifest))
    message("Proceeding with md5sum checks. This may take some time for long file lists/large files...")
    hits <- sapply(check_files, function(x) checkSum(x, manifest, name_col = args$name_col, key_co= args$key_col))
    print(unlist(hits[hits != 1]))
