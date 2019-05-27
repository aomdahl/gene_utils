import argparse
import os

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description = "Makes the descriptors unique in a fasta file by adding a number to the tail of each one")
    parser.add_argument("fasta", help = "The path to the file to uniquify")
    parser.add_argument("-u", "--uniquify", action = "store_true", help = "Use this option to make each label unique.")
    parser.add_argument("-c", "--clean", action = "store_true", help = "Select this option to clean up the labels- remove spaces")
    parser.add_argument("-p", "--punct", action = "store_true", help = "Select this option to remove punctuation from the label.")
    args = parser.parse_args()

    with open(args.fasta, 'r') as istream:
        counter = 0
        for line in istream:
            line =line.strip()
            if line[0] == ">":
                counter += 1
                print_line = line
                if args.uniquify:
                    print_line = print_line + "_" + str(counter)
                if args.clean:
                    print_line = print_line.replace(" ", "_")
                print(print_line)
            else:
                print(line)
