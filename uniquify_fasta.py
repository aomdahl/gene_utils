import argparse
import os

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description = "Makes the descriptors unique in a fasta file by adding a number to the tail of each one")
    parser.add_argument("fasta", help = "The path to the file to uniquify")
    args = parser.parse_args()

    with open(args.fasta, 'r') as istream:
        counter = 0
        for line in istream:
            line =line.strip()
            if line[0] == ">":
                counter += 1
                print(line + "_" + str(counter))
            else:
                print(line)
