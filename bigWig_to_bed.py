#Python2
import sys
import os
import argparse
import pyBigWig

def writeOutput(chr_dict, path):
    with open(path, 'w') as ostream:
        for c in chr_dict:
            for e in chr_dict[c]:
                ostream.write('\t'.join(e) + '\n')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description = "Quick tool to turn a big_wig file into a bed file. Optimized for ARGWeaver BW files")
    parser.add_argument("bw_path", help = "Path to bigwig file to parse")
    parser.add_argument("output", help = "output path")
    args = parser.parse_args() 
    bw = pyBigWig.open(args.bw_path)
    if bw.isBigWig():
        chroms = bw.chroms().keys()
        chr_dict = dict()
        for chrm in chroms:
            t = bw.intervals(chrm)
            score = t[0][2]
            start = t[0][0]
            end = t[0][1]
            con_dat = list()
            for i in range(1,len(t)):
                if t[i][2] == score:
                    end = t[i][1]
                else:
                    #record the information, move on to the next
                    con_dat.append([chrm, str(start), str(end),"tmrca", str(score)])
                    score = t[i][2]
                    start = t[i][0]
                    end = t[i][1]
            con_dat.append([chrm,str(start),str(end), "tmrca", str(score)])
            chr_dict[chrm] = con_dat
            print "Have loaded data from chromosome", chrm
    writeOutput(chr_dict, args.output)
    print "Write out to", args.output, "complete"

