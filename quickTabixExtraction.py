import argparse
import tabix
from subprocess import Popen, PIPE
if __name__ == '__main__':

    parser = argparse.ArgumentParser(description = "Quick script to parse tabix and get the columns that you want")
    parser.add_argument("-stat", help = "Name of summary statistic you'd like to get out",choices = ["ZSCORE", "BETA"], required = True)
    parser.add_argument("--query_file", help = "Contains the queries for the tabix")
    parser.add_argument("--file_list", help = "Specify which files to run the queries on")
    parser.add_argument("-o", "--output", help = "Specify where you would like the output file to be written and its name.")
    args = parser.parse_args()

    
    with open(args.query_file, 'r') as istream:
        q = [line.strip() for line in istream]
    with open(args.file_list, 'r') as istream:
        file_list = [line.strip() for line in istream]
    STAT=args.stat
    ret_dict = {"variant" : q}
    for curr_file in file_list:
        prefix = curr_file.split(".")[0]
        print(curr_file)
        process = Popen(["tabix","-R", args.query_file, curr_file], stdout=PIPE) 
        tab = [line.strip().split() for line in process.stdout] 
        #Better way would be to parse the list just once....
        lookup = {"ZSCORE":9, "BETA":7}
        i = 1
        if len(tab[0]) == 13:
            i=lookup[STAT]
        elif len(tab[0]) == 14:
            i=lookup[STAT] + 1
        else:
            print("Not sure what is going on")

        ret_dict[prefix] = [t[i].decode("utf-8") for t in tab]
    
    import pandas as pd
    f = pd.DataFrame.from_dict(ret_dict)
    f.to_csv(args.output,index = False, sep = '\t')
