#This was custom made for extracting data from the openCravat toolbox
#Based on a script I found on github, but modified for my purpose
import sqlite3
import sys
if sys.argv[1] == "-h" or sys.argv[1] == "--help":
    print("Simple template  script to parse a SQL database table and write it as a TSV. Probably won't work for exactly your application, but its a start.")
    print("First position argument: sqlite database file")
    print("Second position argument: output file name")
out_name = sys.argv[2]
db_file = sys.argv[1]
try:
    conn = sqlite3.connect(db_file)
except Error as e:
    print(e)
cur = conn.cursor()
cursor = cur.execute("SELECT * FROM variant")
names = list(map(lambda x: x[0], cursor.description))

#Store into a map

fin_map = dict()
print("populating feature map...")
for n in names:
    print(n)
    fin_map[n] = list()
    cursor = cur.execute("SELECT " + n + " FROM variant")
    sample_list = list()
    for c in cursor: 
        fin_map[n].append(c[0])


#Write it out

import pandas as pd
to_write = pd.DataFrame.from_dict(fin_map)
drop_cols = ['base__uid','base__all_mappings','flank_seq__ref_seq','flank_seq__alt_seq','tagsampler__numsample','tagsampler__samples']
to_write = to_write.drop(columns = drop_cols)
to_write.to_csv(out_name,sep="\t")
