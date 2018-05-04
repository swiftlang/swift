#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import re
import subprocess
import sys


def run_command(cmd):
    print(' '.join(cmd))
    return subprocess.check_output(cmd, stderr=subprocess.STDOUT)


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='Utility for testing incremental syntax parsing',
        epilog='''
    Based of a single template the utility generates a pre-edit and a post-edit 
    file. It then verifies that incrementally parsing the post-edit file base 
    on the pre-edit file results in the same syntax tree as reparsing the 
    post-edit file from scratch.

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
        '--temp-dir', required=True,
        help='A temporary directory where pre-edit and post-edit files can be \
              saved')
    parser.add_argument(
        '--swift-syntax-test', required=True,
        help='The path to swift-syntax-test')
    parser.add_argument(
        '--test-case', default='',
        help='The test case to execute. If no test case is specified all \
              unnamed substitutions are applied')

    # Parse the arguments
    args = parser.parse_args(sys.argv[1:])
    test_file = args.file
    test_file_name = os.path.basename(test_file.name)
    temp_dir = args.temp_dir
    swift_syntax_test = args.swift_syntax_test
    test_case = args.test_case

    # Generate pre-edit and post-edit files
    pre_edit_file = open(temp_dir + '/' + test_file_name + '.' + test_case + 
                         '.pre.swift', mode='w+')
    post_edit_file = open(temp_dir + '/' + test_file_name + '.' + test_case + 
                          '.post.swift', mode='w+')

    # Gather command line arguments for swift-syntax-test specifiying the 
    # performed edits in this list
    incremental_edit_args = []

    # The regular expression to match the template markers
    subst_re = re.compile(r'^(.*)<<(.*)<(.*)\|\|\|(.*)>>>(.*)')

    line_no = 1
    for line in args.file.readlines():
        match = subst_re.match(line)
        if match:

            prefix = match.group(1)
            match_test_case = match.group(2)
            pre_edit = match.group(3)
            post_edit = match.group(4)
#            suffix = match.group(5)

            if match_test_case == test_case:
                pre_edit_line = subst_re.sub('\\1\\3\\5', line)
                post_edit_line = subst_re.sub('\\1\\4\\5', line)

                if subst_re.match(pre_edit_line):
                    # It probably won't be hard to support multiple edits per
                    # line, we would just need to figure out the column offsets
                    print('Error: Multiple substitutions per line are not '
                          'supported yet by incrparse_test_util.py',
                          file=sys.stderr)
                    sys.exit(1)

                # Compute the -incremental-edit argument for swift-syntax-test
                column = len(prefix) + 1
                edit_arg = '%d:%d-%d:%d=%s' % \
                    (line_no, column, line_no, column + len(pre_edit), 
                     post_edit)
                incremental_edit_args.append('-incremental-edit')
                incremental_edit_args.append(edit_arg)
            else:
                # For different test cases just take the pre-edit text
                pre_edit_line = subst_re.sub('\\1\\3\\5', line)
                post_edit_line = subst_re.sub('\\1\\3\\5', line)
        else:
            pre_edit_line = line
            post_edit_line = line            

        pre_edit_file.write(pre_edit_line)
        post_edit_file.write(post_edit_line)

        line_no += 1

    pre_edit_file.close()
    post_edit_file.close()

    serialized_pre_edit_filename = pre_edit_file.name + '.serialized.json'
    serialized_incr_filename = temp_dir + '/' + test_file_name + '.' + \
        test_case + '.incr.swift.serialized.json'
    serialized_post_edit_filename = post_edit_file.name + '.serialized.json'

    try:
        # Serialise the pre-edit syntax tree
        run_command(
            [
                swift_syntax_test,
                '-serialize-raw-tree',
                '-input-source-filename',
                pre_edit_file.name,
                '-output-filename',
                serialized_pre_edit_filename
            ])

        # Serialise the post-edit syntax tree from scratch
        run_command(
            [
                swift_syntax_test,
                '-serialize-raw-tree',
                '-input-source-filename',
                post_edit_file.name,
                '-output-filename',
                serialized_post_edit_filename
            ])

        # Serialise the post-edit syntax tree incrementally based on the 
        # pre-edit syntax tree
        run_command(
            [
                swift_syntax_test,
                '-serialize-raw-tree',
                '-old-syntax-tree-filename',
                serialized_pre_edit_filename,
                '-input-source-filename',
                post_edit_file.name,
                '--old-source-filename',
                pre_edit_file.name,
                '-output-filename',
                serialized_incr_filename
            ] + incremental_edit_args)

    except subprocess.CalledProcessError as e:
        print("FAILED", file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)

    try:
        # Check if the two syntax trees are the same
        run_command(
            [
                'diff',
                serialized_post_edit_filename,
                serialized_incr_filename
            ])
    except subprocess.CalledProcessError as e:
        print('Syntax tree of incremental compilation does not match '
              'from-scratch parsing of post-edit file:\n\n', file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
