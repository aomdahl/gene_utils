#!/usr/bin/env python

import sys
import os
import argparse
import collections

MAP_INDEX = 0
HEADER_INDEX = 1

#TODO:
#Add functionality for removing repeated columns (i.e. entries match in all cases)
#Add functionality to do full_join

#Parses the args parsing tool and identifies what the right column for joining to use is
def findIDIndex(args, spot):
	#May need to peek at the first line..
	if args.index:
		return int(args.index)
	if args.name:
		with open(args.first, 'r') as f:
				topLine = f.readline().strip().split(args.delim)
				#print "ID index", topLine.index(args.name)
				return topLine.index(args.name)
	if spot == 1:
		if args.index1:
			return args.index1
		if args.name1:
			with open(args.first, 'r') as f:
				topLine = f.readline().strip().split(args.delim)
				return topLine.index(args.name1)
				
	if spot == 2:
		if args.index2:
			return args.index2
		if args.name2:
			with open(args.second, 'r') as f:
				topLine = f.readline().strip().split(args.delim)
				index_two = int(args.name2)
				return topLine.index(index_two)
	
	#Get to this case if nothing specified
	tl1 = []
	tl2  = []
	with open(args.first, 'r') as f:
		tl1 = f.readline().strip().split(args.delim)
	with open(args.second, 'r') as f:
			tl2 = f.readline().strip().split(args.delim)
	for i in tl1:
		if i in tl2:
			return tl1.index(i)
	
#This is the one used in printing. Defaults to what is given in the first table
def findIDName(args):
	if args.name:
		return args.name
	if args.name1:
		return args.name1
	
	#Get to this case if nothing specified
	tl1 = []
	tl2  = []
	with open(args.first, 'r') as f:
		curr_line = f.readline().strip()
		while(curr_line[0] == "#"):
			curr_line = f.readline().strip()
		tl1 = curr_line.split(args.delim)
		
		if args.index1:
			return tl1[args.index1]
				
	with open(args.second, 'r') as f:
		curr_line = f.readline().strip()
		while(curr_line[0] == "#"):
			curr_line = f.readline().strip()
		
		tl2 = curr_line.split(args.delim)
		
	for i in tl1:
		if i in tl2:
			return i

#Prints the header
#Order is Id, data from first table, data from 2nd table.
def printHeaderLine(idName, header1, header2, delim, listData = None):
	
	printStr =  idName + delim + delim.join(header1) + delim + delim.join(header2)
	
	if listData:
		for entry in listData:
			printStr = printStr + delim + delim.join(entry[HEADER_INDEX])
	print(printStr)
	
#This prints the row data
def printRowData(identifier,first, second, delim, header2Length, listData = None):
	prtStr = ""
	if listData:
		if identifier in second:
			prtStr = identifier + delim + delim.join(first[identifier]) + delim + delim.join(second[identifier])
		else:
			prtStr = identifier + delim + delim.join(first[identifier]) + delim + delim * header2Length
			
		for entry in listData:
			if identifier in entry[MAP_INDEX]:
				prtStr = prtStr + delim + delim.join(entry[MAP_INDEX][identifier])
			else:
				print(identifier, entry)
				input("missing")
				prtStr = prtStr + delim + delim * len(entry[HEADER_INDEX])
	else:
		if identifier in second:
			prtStr = identifier + delim + delim.join(first[identifier]) + delim + delim.join(second[identifier])
		else:
			prtStr = identifier + delim + delim.join(first[identifier]) + delim + delim * header2Length
	
	print(prtStr)

def populateMap(filepath, indexName, indexID):
	retDict = dict()
	with open(filepath, 'r') as fStream:
		lineCount = 0
		for line in fStream:
			line = line.strip()
			if lineCount == 0:
				headers = line.split(args.delim)
				del headers[idIndex2]
				
			else:
				data = line.split(args.delim)
				try:
					identifier = data[indexID]
					del data[indexID]
				except IndexError:
					print("error case")
					print(data)
					input()
					pass
			##This needs more work. Not robust enough
				
				#print the data in real time. We want this to be quick.
				retDict[identifier] = data
			lineCount += 1
	return [retDict, headers]
	


#Start with the standard argParser
if __name__ == '__main__':
	parser = argparse.ArgumentParser(description = "This tool allows you to easily map one file onto another based on a ID. This implementation requires unique identifiers")
	parser.add_argument("first", help = "The first file you wish to map with")
	parser.add_argument("second", help= "The second file you wish to map with")
	parser.add_argument("-i1", "--index1", type=int, help = "specify the index of the ID column to combined by in file 1. 0 indexed")
	parser.add_argument("-i2", "--index2",type=int, help = "specify the index of the ID column to combined by in file 2. 0 indexed")
	parser.add_argument("-i", "--index", help = "specify the index of the ID column to combined by- same in both files. 0 indexed")
	parser.add_argument("-n1", "--name1", help = "specify the name of the ID column to combined by in file 1")
	parser.add_argument("-n2", "--name2", help = "specify the name of the ID column to combined by in file 2")
	parser.add_argument("-n", "--name", help = "specify the name of the ID column to combined by- same in both files.")
	parser.add_argument("-ij", "--full_join", action='store_true', help = "Use this if you wish to include all ids from both files. By default only does left inner join.")
	parser.add_argument("-d", "--delim", default = '\t', help = "Specify the file delimiter to use. Must be the same for both files")
	parser.add_argument("-l", "--flist", help = "Path to a file containing a list of files to parse through. Give each file path on a new line. Still need first and second, just omit those from the file..")
	args = parser.parse_args()
	#Iterate through the first file, store 
	first = collections.OrderedDict()
	second = dict()
	headers1 = list()
	headers2 = list()
	idIndex1 = findIDIndex(args, 1) 
	idName = findIDName(args) 
	#Iterate through the first file
	with open(args.first, 'r') as fStream:
		lineCount = 0
		for line in fStream:
			line = line.strip()
			if line[0] == "#": #Its just a comment
				continue
			if lineCount == 0:
				headers1 = line.split(args.delim)
				del headers1[idIndex1]
			else:
				data = line.split(args.delim)
				try:
					identifier = data[idIndex1]
				except:
					print(data)
					input()
				del data[idIndex1]
				first[identifier] = data
			lineCount += 1


	idIndex2 = findIDIndex(args, 2)
	#Iterate through the second, update the map LIVE TIME, default NOT inner join
	
	with open(args.second, 'r') as fStream:
		lineCount = 0
		for line in fStream:
			line = line.strip()
			if line[0] == "#":
				continue
			if lineCount == 0:
				headers2 = line.split(args.delim)
				del headers2[idIndex2]
				
			else:
				data = line.split(args.delim)
				try:
					identifier = data[idIndex2]
					del data[idIndex2]
				except IndexError:
					print("error case")
					print(data)
					input()
					pass
			##This needs more work. Not robust enough
				
				#print the data in real time. We want this to be quick.
				second[identifier] = data
			lineCount += 1
	
	###In the case of multiple files.
	fileList = []
	fileMaps = []
	if args.flist:
		if not args.name and not args.index:
			print("Need a common column to join by, please specify either its name or index")
			sys.exit()
		
		with open(args.flist, 'r') as fStream:
			for line in fStream:
				line = line.strip()
				if line != args.first and line != args.second:
					fileList.append(line)
	for fn in fileList:
		fileMaps.append(populateMap(fn, idName, idIndex1))
	
	#Iterate through the remaining dictionary and print everything we missed on the first go
	printHeaderLine(idName, headers1, headers2, args.delim, listData = fileMaps)


	for item in first:
		if args.flist:
			printRowData(item, first, second, args.delim, len(headers2), listData = fileMaps)
		else:
			printRowData(item, first, second, args.delim, len(headers2))
		
	
		


	

