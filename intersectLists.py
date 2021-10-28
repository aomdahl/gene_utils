import sys
import os
#Quickly intersect lists by a specified column. Allows for large files, since does it line by line
#@arg dir_path: directory
dir_path_first=sys.argv[1]
if dir_path_first == "-h" or dir_path_first == "--help":
    print("this utility will quickly intersect the IDS of twoo tsv files by a specified column. IDs must be present in both files.")
    print("Arguments are \n1) first file \n2) second file \n3)column index of interest \n4) output file") 
    sys.exit()


dir_path_second=sys.argv[2]
col_index = int(sys.argv[3])
output=sys.argv[4]
print("output is " + output)
var_set = dict()
with open(dir_path_first, 'r') as istream:
    for line in istream:
        dat = line.strip().split()
        var_set[dat[col_index]] = ""
with open(dir_path_second, 'r') as istream:
    with open(output, 'w') as ostream:
        for line in istream:
            dat = line.strip().split()
            if dat[col_index] in var_set:
                ostream.write(dat[col_index] + "\n")
