import sys
import os

def debugPrintMap(obj, lines = 10):
    i = 0
    for j in obj:
        print(obj[j])
        i +=1
        if i > lines:
            return

def debuPrintSet(obj, lines = 10):
	i = 0:
	for j in obj:
		print j
		j += 1
		if j > lines:
			return
