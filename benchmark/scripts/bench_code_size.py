#!/usr/bin/env python
# -*- coding: utf-8 -*-

# ===--- bench_code_size -------------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//
#
# Reports code size differences of two benchmark build directories
#
# ===---------------------------------------------------------------------===//

from __future__ import print_function

import argparse
import glob
import os
import subprocess
import sys


def main():
    argparser = argparse.ArgumentParser()
    argparser.add_argument(
        '-O', action='append_const', const='O', dest='opt_levels',
        help='report code size of -O benchmarks')
    argparser.add_argument(
        '-Osize', action='append_const', const='Osize', dest='opt_levels',
        help='report code size of -Osize benchmarks')
    argparser.add_argument(
        '-Onone', action='append_const', const='Onone', dest='opt_levels',
        help='report code size of -Onone benchmarks')
    argparser.add_argument(
        '-swiftlibs', action='append_const', const='swiftlibs',
        dest='opt_levels',
        help='report code size of swift dylibs')
    argparser.add_argument(
        'oldbuilddir', nargs=1, type=str,
        help='old benchmark build directory')
    argparser.add_argument(
        'newbuilddir', nargs=1, type=str,
        help='new benchmark build directory')
    argparser.add_argument(
        '-platform', type=str,
        help='The benchmark build platform', default='macosx')
    args = argparser.parse_args()

    for opt_level in args.opt_levels or ['O', 'Osize', 'Onone']:
        report_code_size(opt_level, args.oldbuilddir[0], args.newbuilddir[0],
                         args.platform)

    return 0


def report_code_size(opt_level, old_dir, new_dir, platform):
    def log_filename(bench_dir):
        return os.path.join(bench_dir, 'result_' + opt_level + '_size')

    old_logf = open(log_filename(old_dir), 'w')
    new_logf = open(log_filename(new_dir), 'w')

    if opt_level == 'swiftlibs':
        files = glob.glob(os.path.join(old_dir, 'lib', 'swift', platform,
                                       '*.dylib'))
    else:
        files = glob.glob(os.path.join(old_dir,
                                       opt_level + '-*' + platform + '*',
                                       '*.o'))

    idx = 1
    for oldfile in files:
        newfile = oldfile.replace(old_dir, new_dir, 1)
        if os.path.isfile(newfile):
            oldsize = get_codesize(oldfile)
            newsize = get_codesize(newfile)
            bname = os.path.basename(oldfile)

            def write_line(value, logf):
                v = ',' + str(value)
                logf.write(str(idx) + ',' + bname + ',1' + (v * 3) +
                           ',0' + v + '\n')

            write_line(oldsize, old_logf)
            write_line(newsize, new_logf)
            idx += 1

    old_logf.close()
    new_logf.close()

    print('Logfiles written to ' + log_filename(old_dir) + ' and ' +
          log_filename(new_dir))


def get_codesize(filename):
    output = subprocess.check_output(['size', filename]).splitlines()
    header_line = output[0]
    data_line = output[1]
    if header_line.find('__TEXT') != 0:
        sys.exit('unexpected output from size command:\n' + output)
    return int(data_line.split('\t')[0])


if __name__ == '__main__':
    sys.exit(main())
