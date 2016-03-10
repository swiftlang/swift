#!/usr/bin/env python

"""
split_file.py [-o <dir>] <path>

Take the file at <path> and write it to multiple files, switching to a new file
every time an annotation of the form "// BEGIN file1.swift" is encountered. If
<dir> is specified, place the files in <dir>; otherwise, put them in the
current directory.
"""

import getopt
import os
import re
import sys


def usage():
    sys.stderr.write(__doc__.strip() + "\n")
    sys.exit(1)

fp_out = None
dest_dir = '.'

try:
    opts, args = getopt.getopt(sys.argv[1:], 'o:h')
    for (opt, arg) in opts:
        if opt == '-o':
            dest_dir = arg
        elif opt == '-h':
            usage()
except getopt.GetoptError:
    usage()

if len(args) != 1:
    usage()
fp_in = open(args[0], 'r')

for line in fp_in:
    m = re.match(r'^//\s*BEGIN\s+([^\s]+)\s*$', line)
    if m:
        if fp_out:
            fp_out.close()
        fp_out = open(os.path.join(dest_dir, m.group(1)), 'w')
    elif fp_out:
        fp_out.write(line)

fp_in.close()
if fp_out:
    fp_out.close()
