#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import re
import subprocess
import sys


def escapeCmdArg(arg):
    if '"' in arg or ' ' in arg:
        return '"%s"' % arg.replace('"', '\\"')
    else:
        return arg


def fail(errorMessage):
    print(errorMessage, file=sys.stderr)
    sys.exit(1)


def run_command(cmd):
    print(' '.join([escapeCmdArg(arg) for arg in cmd]))
    return subprocess.check_output(cmd, stderr=subprocess.STDOUT)


def parseLine(line, line_no, test_case, incremental_edit_args, reparse_args, 
              current_reparse_start):
    pre_column_offset = 1
    post_column_offset = 1
    pre_edit_line = ""
    post_edit_line = ""

    # We parse one tag at a time in the line while eating away a prefix of the 
    # line
    while line:
        # The regular expression to match the template markers
        subst_re = re.compile(r'^(.*?)<<(.*?)<(.*?)\|\|\|(.*?)>>>(.*\n?)')
        reparse_re = re.compile(r'^(.*?)<(/?)reparse ?(.*?)>(.*\n?)')
        subst_match = subst_re.match(line)
        reparse_match = reparse_re.match(line)
        if subst_match and reparse_match:
            # If both regex match use the one with the shorter prefix
            if len(subst_match.group(1)) < len(reparse_match.group(1)):
                reparse_match = None
            else:
                subst_match = None

        if subst_match:
            prefix = subst_match.group(1)
            match_test_case = subst_match.group(2)
            pre_edit = subst_match.group(3)
            post_edit = subst_match.group(4)
            suffix = subst_match.group(5)

            if match_test_case == test_case:
                pre_edit_line += prefix + pre_edit
                post_edit_line += prefix + post_edit

                # Compute the -incremental-edit argument for swift-syntax-test
                column = pre_column_offset + len(prefix)
                edit_arg = '%d:%d-%d:%d=%s' % \
                    (line_no, column, line_no, column + len(pre_edit), 
                     post_edit)
                incremental_edit_args.append('-incremental-edit')
                incremental_edit_args.append(edit_arg)
            else:
                # For different test cases just take the pre-edit text
                pre_edit_line += prefix + pre_edit
                post_edit_line += prefix + pre_edit

            line = suffix
            pre_column_offset += len(pre_edit_line)
            post_column_offset += len(post_edit_line)
        elif reparse_match:
            prefix = reparse_match.group(1)
            is_closing = len(reparse_match.group(2)) > 0
            match_test_case = reparse_match.group(3)
            suffix = reparse_match.group(4)
            if match_test_case == test_case:
                column = post_column_offset + len(prefix)
                if is_closing:
                    if not current_reparse_start:
                        fail('Closing unopened reparse tag in line %d' %
                             line_no)
                    reparse_args.append('-reparse-region')
                    reparse_args.append(
                        '%d:%d-%d:%d' % (current_reparse_start[0], 
                                         current_reparse_start[1], 
                                         line_no, column))
                    current_reparse_start = None
                else:
                    if current_reparse_start:
                        fail('Opening nested reparse tags for the same test '
                             'case in line %d' % line_no)
                    current_reparse_start = [line_no, column]

            pre_edit_line += prefix
            post_edit_line += prefix
            line = suffix
        else:
            pre_edit_line += line
            post_edit_line += line
            # Nothing more to do
            line = ''

    return (pre_edit_line, post_edit_line, current_reparse_start)


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
    parser.add_argument(
        '--print-visual-reuse-info', default=False, action='store_true',
        help='Print visual reuse information about the incremental parse \
              instead of diffing the syntax trees. This option is intended \
              for debug purposes only.')

    # Parse the arguments
    args = parser.parse_args(sys.argv[1:])
    test_file = args.file
    test_file_name = os.path.basename(test_file.name)
    temp_dir = args.temp_dir
    swift_syntax_test = args.swift_syntax_test
    test_case = args.test_case
    print_visual_reuse_info = args.print_visual_reuse_info

    # Generate pre-edit and post-edit files
    pre_edit_file = open(temp_dir + '/' + test_file_name + '.' + test_case + 
                         '.pre.swift', mode='w+')
    post_edit_file = open(temp_dir + '/' + test_file_name + '.' + test_case + 
                          '.post.swift', mode='w+')

    # Gather command line arguments for swift-syntax-test specifiying the 
    # performed edits in this list
    incremental_edit_args = []
    reparse_args = []
    current_reparse_start = None

    line_no = 1
    for line in args.file.readlines():
        parseLineRes = parseLine(line, line_no, test_case, 
                                 incremental_edit_args,
                                 reparse_args, current_reparse_start)
        (pre_edit_line, post_edit_line, current_reparse_start) = parseLineRes

        pre_edit_file.write(pre_edit_line)
        post_edit_file.write(post_edit_line)

        line_no += 1

    if current_reparse_start:
        fail('Unclosed reparse tag for test case %s' % test_case)

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

        if print_visual_reuse_info:
            print_visual_reuse_info_args = [
                '-print-visual-reuse-info', 
                '-force-colored-output'
            ]
        else:
            print_visual_reuse_info_args = []

        # Serialise the post-edit syntax tree incrementally based on the 
        # pre-edit syntax tree
        incr_parse_output = run_command(
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
            ] + 
            incremental_edit_args + 
            reparse_args + 
            print_visual_reuse_info_args)

        if print_visual_reuse_info:
            print(incr_parse_output)
            exit(0)

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
        print('Test case %s failed' % (test_case), file=sys.stderr)
        print('Syntax tree of incremental parsing does not match '
              'from-scratch parsing of post-edit file:\n\n', file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
