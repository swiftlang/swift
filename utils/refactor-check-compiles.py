#!/usr/bin/env python
from __future__ import print_function

import argparse
import os
import subprocess
import sys


def run_cmd(cmd, desc):
    try:
        return subprocess.check_output(cmd)
    except subprocess.CalledProcessError:
        print('FAILED ' + desc + ':', file=sys.stderr)
        print(' '.join(cmd), file=sys.stderr)
        sys.exit(1)


def parse_args():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""
        A drop-in replacement for a 'swift-refactor -dump-text' call that
        1. Checks that the file still compiles after the refactoring by doing
           'swift-refactor -dump-rewritten' and feeding the result to
           'swift-frontend -typecheck'
        2. Outputting the result of the 'swift-refactor -dump-text' call

        All arguments other than the following will be forwarded to
        'swift-refactor':
         - swift-frontend
         - swift-refactor
         - temp-dir
         - enable-experimental-concurrency (sent to both)
        """)

    parser.add_argument(
        '-swift-frontend',
        help='The path to the swift-frontend executable'
    )
    parser.add_argument(
        '-swift-refactor',
        help='The path to the swift-refactor executable'
    )
    parser.add_argument(
        '-temp-dir',
        help='A temporary directory to save the rewritten file in'
    )
    parser.add_argument(
        '-dump-text',
        action='store_true',
        help='''
        Required argument to indicate that the outputted text will be that of
        swift-refactor -dump-text. This makes this script a drop-in replacement.
        '''
    )
    parser.add_argument(
        '-source-filename',
        help='The file to refactor'
    )
    parser.add_argument(
        '-pos',
        help='The position to invoke the refactoring at'
    )
    parser.add_argument(
        '-enable-experimental-concurrency',
        action='store_true',
        help='''
        Whether to enable experimental concurrency in both swift-refactor and
        swift-frontend
        '''
    )

    return parser.parse_known_args()


def main():
    (args, extra_refactor_args) = parse_args()
    temp_file_name = os.path.split(args.source_filename)[-1] + '.' + \
        args.pos.replace(':', '.')
    temp_file_path = os.path.join(args.temp_dir, temp_file_name)

    extra_frontend_args = []
    if args.enable_experimental_concurrency:
        extra_refactor_args.append('-enable-experimental-concurrency')
        extra_frontend_args.append('-enable-experimental-concurrency')

    dump_text_output = run_cmd([
        args.swift_refactor,
        '-dump-text',
        '-source-filename', args.source_filename,
        '-rewritten-output-file', temp_file_path,
        '-pos', args.pos
    ] + extra_refactor_args, desc='producing edit').decode("utf-8")

    run_cmd([
        args.swift_frontend,
        '-typecheck',
        temp_file_path,
        '-disable-availability-checking'
    ] + extra_frontend_args, desc='checking that rewritten file compiles')
    sys.stdout.write(dump_text_output)


if __name__ == '__main__':
    main()
