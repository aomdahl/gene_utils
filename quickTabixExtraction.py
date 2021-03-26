import os
import argparse
import tabix
from subprocess import Popen, PIPE
if __name__ == '__main__':

    parser = argparse.ArgumentParser(description = "Quick script to parse tabix and get the columns that you want")
    parser.add_argument("-stat", help = "Name of summary statistic you'd like to get out",choices = ["ZSCORE", "BETA", "PVAL", "ZSCORE+PVAL"], required = True)
    parser.add_argument("--query_file", help = "Contains the queries for the tabix")
    parser.add_argument("--file_list", help = "Specify which files to run the queries on")
    parser.add_argument("-o", "--output", help = "Specify where you would like the output file to be written and its name.")
    args = parser.parse_args()

    pos = list()
    chrom = list() 
    with open(args.query_file, 'r') as istream:
        for line in istream:
            line = line.strip().split()
            chrom.append(line[0])
            pos.append(line[1])
    
    with open(args.file_list, 'r') as istream:
        file_list = [line.strip() for line in istream]
    STAT=args.stat
    #ret_dict = {"chr" : chrom, "pos": pos}
    ret_dict = dict()
    first = True
    for curr_file in file_list:
        print(curr_file)
        process = Popen(["tabix","-R", args.query_file, curr_file], stdout=PIPE) 
        tab = [line.strip().split() for line in process.stdout] 
        #Better way would be to parse the list just once....
        lookup = {"ZSCORE":9, "BETA":7, "PVAL":10} #note that the numbers are different from the standard tsv files because of an added index column at the end.
        i = 1
        if(len(tab) == 0):
            print("None of the SNPs were found in query file", curr_file)
            continue
        if len(tab[0]) == 13:
            i=lookup[STAT]
        elif len(tab[0]) == 14:
            i=lookup[STAT] + 1
        else:
            print("Not sure what is going on")
        h = os.path.basename(curr_file)
        xr  = [t[i].decode("utf-8") for t in tab]
        if first:
            snp_ids = [x[0].decode("utf-8") for x in tab] 
            chr_hits = [ x.split(":")[0] for x in snp_ids]
            pos_hits  = [x.split(":")[1] for x in snp_ids]
            ref_hit = [x.split(":")[2] for x in snp_ids]
            alt_hit = [x.split(":")[3] for x in snp_ids]
            ret_dict["chr"] = chr_hits
            ret_dict["pos"] = pos_hits
            ret_dict["chr"] = ref_hit
            ret_dict["alt"] = alt_hit
            first = False
        ret_dict[h.split(".")[0]] = xr
    import pandas as pd
    f = pd.DataFrame.from_dict(ret_dict)
    f.to_csv(args.output,index = False, sep = '\t')
