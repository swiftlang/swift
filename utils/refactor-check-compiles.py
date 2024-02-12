#!/usr/bin/env python3
import argparse
import os
import subprocess
import sys


def run_cmd(cmd, desc):
    try:
        return subprocess.check_output(cmd, universal_newlines=True)
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
           'swift-frontend -typecheck -disable-availability-checking
            -warn-on-editor-placeholder'
        2. Outputting the result of the 'swift-refactor -dump-text' call

        All arguments other than the following will be forwarded to
        'swift-refactor':
         - swift-frontend
         - swift-refactor
         - temp-dir
         - enable-experimental-concurrency (sent to both)
         - I (sent to both)
         - sdk (sent to both)
         - target (sent to both)
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
    parser.add_argument(
        '-I',
        action='append',
        help='Add a directory to the import search path'
    )
    parser.add_argument(
        '-sdk',
        help='Path to the SDK to build against'
    )
    parser.add_argument(
        '-target',
        help='The target triple to build for'
    )
    parser.add_argument(
        '-resource-dir',
        help='The resource dir where the stdlib is stored')

    return parser.parse_known_args()


def main():
    (args, extra_refactor_args) = parse_args()
    temp_file_name = os.path.split(args.source_filename)[-1] + '.' + \
        args.pos.replace(':', '.')
    temp_file_path = os.path.join(args.temp_dir, temp_file_name)

    extra_both_args = []
    if args.enable_experimental_concurrency:
        extra_both_args.append('-enable-experimental-concurrency')
    if args.I:
        for path in args.I:
            extra_both_args += ['-I', path]
    if args.sdk:
        extra_both_args += ['-sdk', args.sdk]
    if args.target:
        extra_both_args += ['-target', args.target]
    if args.resource_dir:
        extra_both_args += ['-resource-dir', args.resource_dir]

    dump_text_output = run_cmd([
        args.swift_refactor,
        '-dump-text',
        '-source-filename', args.source_filename,
        '-rewritten-output-file', temp_file_path,
        '-pos', args.pos
    ] + extra_refactor_args + extra_both_args, desc='producing edit')
    sys.stdout.write(dump_text_output)

    run_cmd([
        args.swift_frontend,
        '-typecheck',
        temp_file_path,
        '-disable-availability-checking',
        '-warn-on-editor-placeholder'
    ] + extra_both_args, desc='checking that rewritten file compiles')


if __name__ == '__main__':
    main()
