#!/usr/bin/env python
import argparse
import sys
parser = argparse.ArgumentParser()
parser.add_argument("lookup_list", help="The list of terms you are looking up")
parser.add_argument("reference_list", help="The list you are referencing from")
parser.add_argument("-p", "--print_index", type=int, default = -1, help = "The index of the data you wish to print, default is all")
parser.add_argument("-n", "--no_match", default = 0, help = "The thing you wish to print out if no match is found.")
parser.add_argument("-s", "--silent", action = "store_true", help= "Specify not to report no match conditions.")
parser.add_argument("-j", "--join", action = "store_true", help = "Join the data at the given keys")
parser.add_argument("-i", "--invert", action = "store_true", help = "Search for all the keys that ARENT in the reference set.")
args = parser.parse_args()

r = dict() #the things we will be referencing
with open(args.reference_list, 'r') as ref:
	for line in ref:
		if line[0] == "#":
			continue
		line = line.strip()
		data = line.split('\t')
		r[data[0]] = data
#print r
#raw_input()
with open(args.lookup_list, 'r') as look:
	for line in look:
		line = line.strip()
		if line[0] == "#":
			continue
		line = line.split("\t")
		l = line[0]
		if l in r:
			if args.invert:
				continue
			if args.print_index == -1:
				print_string = ""
				for entry in r[l]:
					print_string = print_string + entry + '\t'
				for entry in line:
					if entry != l:
						print_string = print_string + entry + '\t'
					
				print(print_string[:-1])
			else:
				print(r[l][args.print_index])
		else:
			if args.invert:
				print(l)
				continue
			if not args.silent:
				print(l, "no match")
