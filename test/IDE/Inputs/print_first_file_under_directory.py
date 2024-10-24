#!/usr/bin/env python3
#
# Usage: print_first_file_under_director.py /dir/

import os
import sys

directory = sys.argv[1]

for p in os.listdir(directory):
    with open(directory + '/' + p, 'r') as f:
        print(f.read())
    exit(0)

exit(1)
