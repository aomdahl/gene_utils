# gene_utils
Simple scripts for basic bioinformatics/genomics related operations. Most or all are still in development

### r_util.R
Some basic functions I perform frequently in R. Just need to import this into R script and voila

### find_genes_in_window.py
This generates a list of genes and all the corresponding genes within a specified window of that gene. i.e. creates a list of all genes from input file within 1MB of each other. Can write out (-p) or store in a serialized hash table for fast access.
See find_genes_in_window.py -h for all arguments

### debug_tools.py
A random set of tools to help in debugging in python. Still in development through

### map_util.py
Purpose is to let you perform advanced joining operations on files based on a specified column of IDs shared between them. Still has some kinks in it to get full flexibility.

### match_IDs.py
A simpler version of map_util.py that simply matches all the IDs in a lookup list with those in a reference list.

### bash_functions.py
Some basic bash functions for ease at the command line that I might use across machines.

### quickHist.R
Quickly generate a histogram of data in R. Can specify which column you want, reads it into a TSV and boom. Should be pretty simple.

###bigWig_to_bed.py
Convert from a bigWig file to a bed file. Runs python 2, optimized for a specific use but easily modified for general case

### long_range_ld.txt
This file contains the regions of long-range LD (from chromosomal inversions?) identified by the UKBB in "The uk biobank resource with deep phenotyping and genomic data."
See https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-018-0579-z/MediaObjects/41586_2018_579_MOESM1_ESM.pdf, table S13. Positions are based in GRCh37.
## TODO:
-Fix map_util so it doesn't throw errors when using a  column name to specify where
- MOve gene_utils from attenuata work into here?
- Update bash_functions.py from .bashrc
