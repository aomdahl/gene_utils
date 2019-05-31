#This script came from Will submitted to https://www.biostars.org/p/1852/
import sys
from itertools import chain, groupby, imap, islice

def fasta_iter(handle):
    """Yields headers and sequences as generators."""
    line_iter = imap(lambda x: x.strip(), handle)
    for is_header, group in groupby(line_iter, lambda x: x.startswith('>')):
        if is_header:
            header = group.next()
        else:
            seq = chain.from_iterable(group)
            yield header, seq

def take(n, iterable):
    return list(islice(iterable, n))

def digest(in_file, width, out_file):

    for header, seq in fasta_iter(in_file):
        siter = iter(seq)       
        block = ''.join(take(width, siter))
        c = 0
        while len(block):
            c += 1
            print c
            out_file.write('%s_%06d\n%s\n' % (header, c, block))    
            block = ''.join(take(width, siter))

if __name__ == '__main__':
    args = sys.argv[1:]
    ifile = args[0]
    width = int(args[1])
    ofile = args[2]
    with open(ifile) as ihandle:
        with open(ofile, 'w') as ohandle:
            digest(ihandle, width, ohandle)
