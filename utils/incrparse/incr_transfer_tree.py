#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import subprocess
import sys

from test_util import TestFailedError, run_command, \
    serializeIncrParseMarkupFile


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='Utility for testing incremental syntax tree transfer',
        epilog='''
    Based of a single template the utility generates a pre-edit and a post-edit
    file. It then verifies that the incrementally transferred syntax tree
    matches the syntax tree passed as --expected-incremental-syntax-tree.

    To generate the pre-edit and the post-edit file from the template, it
    operates on markers of the form:

        <<test_case<pre|||post>>>

    These placeholders are replaced by:
      - 'pre' if a different test case than 'test_case' is run
      - 'pre' for the pre-edit version of 'test_case'
      - 'post' for the post-edit version of 'test_case'
    ''')
    parser.add_argument(
        'file', type=argparse.FileType(),
        help='The template file to test')
    parser.add_argument(
        '--test-case', default='',
        help='The test case to execute. If no test case is specified all '
             'unnamed substitutions are applied')
    parser.add_argument(
        '--temp-dir', required=True,
        help='A temporary directory where pre-edit and post-edit files can be '
             'saved')
    parser.add_argument(
        '--swift-syntax-test', required=True,
        help='The path to swift-syntax-test')
    parser.add_argument(
        '--expected-incremental-syntax-tree', required=True,
        help='The path to a file that contains the expected incrementally '
             'transferred syntax tree')

    args = parser.parse_args(sys.argv[1:])

    test_file = args.file.name
    test_file_name = os.path.basename(test_file)
    test_case = args.test_case
    temp_dir = args.temp_dir
    swift_syntax_test = args.swift_syntax_test
    expected_syntax_tree_file = args.expected_incremental_syntax_tree

    if not os.path.exists(temp_dir):
        os.makedirs(temp_dir)

    incremental_serialized_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.incr.json'

    try:
        serializeIncrParseMarkupFile(test_file=test_file,
                                     test_case=test_case,
                                     mode='incremental',
                                     serialization_mode='incremental',
                                     serialization_format='json',
                                     omit_node_ids=False,
                                     output_file=incremental_serialized_file,
                                     temp_dir=temp_dir + '/temp',
                                     swift_syntax_test=swift_syntax_test,
                                     print_visual_reuse_info=False)
    except TestFailedError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print(e.message, file=sys.stderr)
        sys.exit(1)

    # Check if the two syntax trees are the same
    try:
        run_command(
            [
                'diff', '-u',
                incremental_serialized_file,
                expected_syntax_tree_file
            ])
    except subprocess.CalledProcessError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print('Syntax tree of incremental parsing does not match expected '
              'incrementally transfer syntax tree:\n\n', file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
