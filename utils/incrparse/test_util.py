#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import re
import subprocess
import sys


class TestFailedError(Exception):
    pass


def escapeCmdArg(arg):
    if '"' in arg or ' ' in arg:
        return '"%s"' % arg.replace('"', '\\"')
    else:
        return arg


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
                        raise TestFailedError('Closing unopened reparse tag '
                                              'in line %d' % line_no)
                    reparse_args.append('-reparse-region')
                    reparse_args.append(
                        '%d:%d-%d:%d' % (current_reparse_start[0], 
                                         current_reparse_start[1], 
                                         line_no, column))
                    current_reparse_start = None
                else:
                    if current_reparse_start:
                        raise TestFailedError('Opening nested reparse tags '
                                              'for the same test case in line '
                                              '%d' % line_no)
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


def serializeIncrParseMarkupFile(test_file, test_case, mode, 
                                 serialization_mode, serialization_format, 
                                 omit_node_ids, output_file, temp_dir, 
                                 swift_syntax_test, print_visual_reuse_info):
    test_file_name = os.path.basename(test_file)
    pre_edit_file = temp_dir + '/' + test_file_name + '.' + test_case + \
        '.pre.swift'
    post_edit_file = temp_dir + '/' + test_file_name + '.' + test_case + \
        '.post.swift'

    if not os.path.exists(temp_dir):
        os.makedirs(temp_dir)

    # =========================================================================
    # First generate the pre-edit and post-edit Swift file and gather the edits
    # and expected reparse regions. This is the parser for the special edit 
    # markup for testing incremental parsing
    # =========================================================================

    with open(test_file, mode='r') as test_file_handle, \
            open(pre_edit_file, mode='w+') as pre_edit_file_handle, \
            open(post_edit_file, mode='w+') as post_edit_file_handle:

        # Gather command line arguments for swift-syntax-test specifiying the 
        # performed edits in this list
        incremental_edit_args = []
        reparse_args = []
        current_reparse_start = None

        line_no = 1
        for line in test_file_handle.readlines():
            parseLineRes = parseLine(line, line_no, test_case, 
                                     incremental_edit_args,
                                     reparse_args, current_reparse_start)
            (pre_edit_line, post_edit_line, current_reparse_start) = \
                parseLineRes

            pre_edit_file_handle.write(pre_edit_line)
            post_edit_file_handle.write(post_edit_line)

            line_no += 1

        if current_reparse_start:
            raise TestFailedError('Unclosed reparse tag for test case %s' %
                                  test_case)

    # =========================================================================
    # Now generate the requested serialized file
    # =========================================================================

    # Build the command to serialize the tree depending on the command line 
    # arguments

    try:
        command = [
            swift_syntax_test, 
            '-serialize-raw-tree',
            '-output-filename', output_file
        ]

        if omit_node_ids:
            command.extend(['-omit-node-ids'])

        if serialization_mode == 'full':
            # Nothing to do. This is the default behaviour of swift-syntax-test
            pass 
        elif serialization_mode == 'incremental':
            command.extend(['-incremental-serialization'])
        else:
            raise ValueError('Unknown serialization mode "%s"' % 
                             serialization_mode)

        if serialization_format == 'json':
            # Nothing to do. This is the default behaviour of swift-syntax-test
            pass 
        elif serialization_format == 'byteTree':
            command.extend(['-serialize-byte-tree'])
        else:
            raise ValueError('Unknown serialization format "%s"' % 
                             serialization_format)

        if mode == 'pre-edit':
            command.extend(['-input-source-filename', pre_edit_file])
        elif mode == 'post-edit':
            command.extend(['-input-source-filename', post_edit_file])
        elif mode == 'incremental':
            # We need to build the syntax tree of the pre-edit file first so 
            # that we can pass it to swift-syntax-test to perform incremental 
            # parsing
            pre_edit_tree_file = pre_edit_file + '.serialized.json'

            run_command([swift_syntax_test] + 
                        ['-serialize-raw-tree'] +
                        ['-input-source-filename', pre_edit_file] +
                        ['-output-filename', pre_edit_tree_file])

            # Then perform incremental parsing with the old syntax tree on the 
            # post-edit file
            command.extend(['-input-source-filename', post_edit_file])
            command.extend(['-old-syntax-tree-filename', 
                            pre_edit_tree_file])
            command.extend(['--old-source-filename', pre_edit_file])
            command.extend(incremental_edit_args)
            command.extend(reparse_args)
            if print_visual_reuse_info:
                command.extend([
                    '-print-visual-reuse-info', 
                    '-force-colored-output'
                ])
        else:
            raise ValueError('Unknown mode "%s"' % mode)

        output = run_command(command)
        if print_visual_reuse_info:
            print(output)
    except subprocess.CalledProcessError as e:
        raise TestFailedError(e.output)


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='Utility for testing incremental syntax parsing',
        epilog='''
    This utility can parse a special markup to dedicate a pre-edit and a 
    post-edit version of a file simulateously and generate a serialized version
    of the libSyntax tree by parsing either the pre-edit file, the post-edit 
    file or the edits that are required to retrieve the post-edit file from the
    pre-edit file incrementally.

    To generate the pre-edit and the post-edit file from the template, it 
    operates on markers of the form:

        <<test_case<pre|||post>>>

    These placeholders are replaced by:
      - 'pre' if a different test case than 'test_case' is run
      - 'pre' for the pre-edit version of 'test_case'
      - 'post' for the post-edit version of 'test_case''')
    parser.add_argument(
        'file', type=argparse.FileType(),
        help='The template file to test')
    parser.add_argument(
        '--test-case', default='',
        help='The test case to execute. If no test case is specified all \
              unnamed substitutions are applied')
    parser.add_argument(
        '--mode', choices=['pre-edit', 'incremental', 'post-edit'], 
        required=True, help='''
    The type of parsing to perform:
    - pre-edit: Serialize the syntax tree when parsing the pre-edit file \
    from scratch
    - incremental: Serialize the syntax tree that results from parsing the \
    edits between the pre-edit and post-edit file incrementally
    - post-edit: Serialize the syntax tree that results from parsing the \
    post-edit file from scratch
    ''')
    parser.add_argument(
        '--serialization-mode', choices=['full', 'incremental'], 
        default='full', help='''
    Only applicable if `--mode` is `incremental`. Whether to serialize the 
    entire tree or use the incremental transfer mode. Default is `full`.
    ''')
    parser.add_argument(
        '--serialization-format', choices=['json', 'byteTree'], 
        default='json', help='''
    The format in which the syntax tree shall be serialized.
    ''')
    parser.add_argument(
        '--omit-node-ids', default=False, action='store_true',
        help='Don\'t include the ids of the nodes in the serialized syntax \
        tree')
    parser.add_argument(
        '--output-file', required=True,
        help='The file to which the serialized tree shall be written.')
    parser.add_argument(
        '--temp-dir', required=True,
        help='A temporary directory where pre-edit and post-edit files can be \
              saved')
    parser.add_argument(
        '--swift-syntax-test', required=True,
        help='The path to swift-syntax-test')
    parser.add_argument(
        '--print-visual-reuse-info', default=False, action='store_true',
        help='Print visual reuse information about the incremental parse \
              instead of diffing the syntax trees. This option is intended \
              for debug purposes only.')

    args = parser.parse_args(sys.argv[1:])

    test_file = args.file.name
    test_case = args.test_case
    mode = args.mode
    serialization_mode = args.serialization_mode
    serialization_format = args.serialization_format
    omit_node_ids = args.omit_node_ids
    output_file = args.output_file
    temp_dir = args.temp_dir
    swift_syntax_test = args.swift_syntax_test
    visual_reuse_info = args.print_visual_reuse_info

    try:
        serializeIncrParseMarkupFile(test_file=test_file, 
                                     test_case=test_case, 
                                     mode=mode, 
                                     serialization_mode=serialization_mode,
                                     serialization_format=serialization_format,
                                     omit_node_ids=omit_node_ids,
                                     output_file=output_file, 
                                     temp_dir=temp_dir, 
                                     swift_syntax_test=swift_syntax_test, 
                                     print_visual_reuse_info=visual_reuse_info)
    except TestFailedError as e:
        print(e.message, file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
