#!/usr/bin/env python

import sys
import os
import argparse
import collections
import pickle

#Generates a strucutre of chr:gene_id:[start, stop, name]
def fileLoadChrHash(ipath, exclude):
    ret_struct = dict() #Organized as chr:
    CHR = 2
    START = 3
    STOP = 4
    ID = 1
    NAME = 0
    with open(ipath, "r") as istream:
        for line in istream:
            dat = line.strip().split('\t')
            if dat[NAME] == "gene":
                continue
            curr_chr = dat[CHR]
            curr_id = dat[ID]
            if curr_chr in exclude: #omit any of a specific chromosome
                continue
            if curr_chr not in ret_struct:
                ret_struct[curr_chr] = dict()
            ret_struct[curr_chr][curr_id] = [dat[START], dat[STOP], dat[NAME]]
            #print(ret_struct)
            #input()
    return ret_struct
           

def between(lookup, start, stop):
    if lookup >= start and lookup <= stop:
        return True
    return False

def within(lookup_start, lookup_stop, start, stop):
    if between(lookup_start, start, stop) and between(lookup_stop, start, stop):
        return True
    return False



def withinBounds(bound, start_ref, stop_ref, start_compare, stop_compare):
    if(abs(start_ref - stop_compare) <= bound):
        return True
    elif(abs(stop_ref - start_compare) <= bound):
        return True
    elif(between(start_compare,start_ref, stop_ref) or between(stop_compare,start_ref, stop_ref)):
        return True
    elif(within(start_compare,stop_compare, start_ref, stop_ref) or within(start_ref, stop_ref, start_compare,stop_compare)):
        return True
    else:
        return False
    return False
#this add an item to a set, where reference is the key in the dictionary and add_in is the item to add to teh set
def safeAdd(dict_in, reference, add_in):
    if reference not in dict_in:
        #dict_in[reference] = list()
        dict_in[reference] = set()
    #dict_in[reference].append(add_in)
    dict_in[reference].add(add_in)


def printResults(res):
    print("chr\tref\tWithinBand")
    for gene in res:
            print_string = gene + '\t'
            for close_gene in res[gene]:
                print_string= print_string + close_gene + ';'
            #for close_gene in res[chr][gene]:
            #    print_string = print_string + close_gene + '\t'
            print(print_string[:-1])

def printMini(res):
    i = 0
    for gene in res:
        print_string = gene + '\t'
        for close_gene in res[gene]:
            print_string= print_string + close_gene + '\t'
            #for close_gene in res[chr][gene]:
            #    print_string = print_string + close_gene + '\t'
            print(print_string[:-1])
        i += 1
        if i > 5:
            return

#A way to call this function from another script
#@param exclusions: a string of chr to omit ("chr1,chr2...")
def buildDistHash(exclusions,fpath, distance):
    exclude = []
    if exclusions:
        exclude = exclusions.split(",")
    START = 0
    STOP = 1
    #Load the data into a dictionary of chr :ict(id: start, stop, name)
    chr_hash = fileLoadChrHash(fpath, exclude)
    chr_bins = dict()
    for chr in chr_hash:
        #chr_bins[chr] = dict()
        for gene in chr_hash[chr]:
            start_c = int(chr_hash[chr][gene][START]) #11/5 Correction, mistakenly put STOP here
            stop_c = int(chr_hash[chr][gene][STOP]) #gets the current gene's stop site.
            if gene not in chr_bins:
                chr_bins[gene] = set()
            for lookup in chr_hash[chr]:
                start_lookup = int(chr_hash[chr][lookup][START])
                stop_lookup = int(chr_hash[chr][lookup][STOP])
                if gene != lookup:
                    if gene == "chr1" or lookup == "chr1":
                        print("culprint finded")
                        input()
                    if withinBounds(distance, start_c, stop_c, start_lookup, stop_lookup):
                        #safeAdd(chr_bins[chr], gene, lookup)
                        #Modified on 10/23 for direct accesss, since gene ids should be unique (?- may want to check this)
                        safeAdd(chr_bins, gene, lookup)
                else:
                    safeAdd(chr_bins, gene,lookup)
    return chr_bins




if __name__ == '__main__':
    parser = argparse.ArgumentParser(description = "This tool generates a data structure containing all genes within a certain distance of each gene, as specified by the user")
    parser.add_argument("gene_list", help = "The gene list in bed-kind of file format you are interested in getting distances for. Cols are gene id chr start end ")
    parser.add_argument("-d", "--distance", type = int, default = 1000000, help = " The distance interval you are interested in finding. Default is 1mb")
    parser.add_argument("-e", "--exclude", help = "List any chromosomes you would like to exclude, if any, from your consideration. Separate by comma.")
    parser.add_argument("-s", "--sorted", action = "store_true", help = "Choose this option if the list is already sorted by location")
    parser.add_argument("-p", "--print", action = "store_true", help = "specify if you wish to print output to command line. Otherwise it will pickle the result in the current directory.")
    parser.add_argument("-o", "--output", help = "specify output directory to print to.", default = "1mb_dist.dump")
    args = parser.parse_args()

    
    chr_bins = buildDistHash(args.exclude, args.gene_list, args.distance)
    if args.print == True:
        printResults(chr_bins)
        #printMini(chr_bins)
    else:
        ostream = open(args.output, 'wb')
        pickle.dump(chr_bins, ostream)
        ostream.close()

