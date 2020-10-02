#!/usr/bin/env python

from __future__ import print_function

import argparse
import io
import json
import os
import sys


# Python 2 `unicode` was renamed `str` in Python 3.  To consistently support
# both, define `unicode` to be `str` when using Python 3.  Once we can drop
# Python 2 support, delete this and change all uses of `unicode` to `str`.
if sys.version_info[0] >= 3:
    unicode = str


def fatal(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


def find_swift_files(path):
    for parent, dirs, files in os.walk(path, topdown=True):
        for filename in files:
            if not filename.endswith('.swift'):
                continue
            yield (parent, filename)


def main(arguments):
    parser = argparse.ArgumentParser(
        description='Generate an output file map for the given directory')
    parser.add_argument('-o', dest='output_dir',
                        help='Directory to which the file map will be emitted')
    parser.add_argument('-r', dest='response_output_file',
                        help="""Directory to which a matching response file
                                will be emitted""")
    parser.add_argument('input_dir', help='a directory of swift files')
    args = parser.parse_args(arguments)

    if not args.output_dir:
        fatal("output directory is required")

    # Create the output directory if it doesn't already exist.
    if not os.path.isdir(args.output_dir):
        os.makedirs(args.output_dir)

    output_path = os.path.join(args.output_dir, 'output.json')

    if not os.path.isdir(args.input_dir):
        fatal("input directory does not exist, or is not a directory")

    swift_files = find_swift_files(args.input_dir)
    if not swift_files:
        fatal("no swift files in the given input directory")

    response_file_contents = []
    all_records = {}
    for (root, swift_file) in swift_files:
        file_name = os.path.splitext(swift_file)[0]
        all_records['./' + swift_file] = {
            'object': './' + file_name + '.o',
            'swift-dependencies': './' + file_name + '.swiftdeps',
        }
        response_file_contents.append(os.path.join(root, swift_file))
    all_records[""] = {
        'swift-dependencies': './main-buildrecord.swiftdeps'
    }

    with io.open(output_path, 'w', encoding='utf-8', newline='\n') as f:
        f.write(unicode(json.dumps(all_records, ensure_ascii=False)))

    if args.response_output_file is not None:
        with io.open(args.response_output_file, 'w',
                     encoding='utf-8', newline='\n') as f:
            for line in response_file_contents:
                f.write(unicode(line + " "))


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
