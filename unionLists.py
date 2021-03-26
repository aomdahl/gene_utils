import sys
import os

dir_path=sys.argv[1]
extension=sys.argv[2]
output=sys.argv[3]
var_set = set()
for file in os.listdir(dir_path):
    if file.endswith(extension):
        with open(dir_path + "/" + file, 'r') as istream:
            lines  = istream.read().splitlines()
            var_set.update(lines)
with open(sys.argv[3], 'w') as ostream:
    for var in var_set:
        ostream.write(var+'\n')
