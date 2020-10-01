#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import subprocess
import sys

from test_util import TestFailedError, prepareForIncrParse, run_command, \
    serializeIncrParseMarkupFile


def testWithParserLib(test_file, test_case, pre_edit_file, post_edit_file,
                      after_roundtrip_file, swiftsyntax_lit_test_helper):
    # =========================================================================
    # First generate the pre-edit and post-edit Swift file and gather the edits
    # and expected reparse regions. This is the parser for the special edit
    # markup for testing incremental parsing
    # =========================================================================

    # Gather command line arguments for swift-syntax-test specifiying the
    # performed edits in this list
    incremental_edit_args = []
    reparse_args = []
    try:
        prepareForIncrParse(test_file=test_file, test_case=test_case,
                            pre_edit_file=pre_edit_file,
                            post_edit_file=post_edit_file,
                            incremental_edit_args=incremental_edit_args,
                            reparse_args=reparse_args)
    except TestFailedError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print(e.message, file=sys.stderr)
        sys.exit(1)

    try:
        run_command([swiftsyntax_lit_test_helper, '-parse-incremental'] +
                    ['-old-source-file', pre_edit_file] +
                    ['-source-file', post_edit_file] +
                    incremental_edit_args + reparse_args +
                    ['-out', after_roundtrip_file])
    except subprocess.CalledProcessError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print('Parsing the swift file failed:\n', file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)

    # Check if the two syntax trees are the same
    try:
        run_command(
            [
                'diff', '-u',
                post_edit_file,
                after_roundtrip_file
            ])
    except subprocess.CalledProcessError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print('Source file after incrementally parsing '
              'does not match post-edit source file:\n\n',
              file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)


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
        '--test-case', default='',
        help='The test case to execute. If no test case is specified all \
              unnamed substitutions are applied')
    parser.add_argument(
        '--temp-dir', required=True,
        help='A temporary directory where pre-edit and post-edit files can be \
              saved')
    parser.add_argument(
        '--swift-syntax-test', required=False,
        help='The path to swift-syntax-test')
    parser.add_argument(
        '--swiftsyntax-lit-test-helper', required=True,
        help='The path to the lit-test-helper binary of SwiftSyntax')
    parser.add_argument(
        '--serialization-format', choices=['json', 'byteTree'],
        default='json', help='''
    The format that shall be used to transfer the syntax tree
    ''')

    args = parser.parse_args(sys.argv[1:])

    test_file = args.file.name
    test_file_name = os.path.basename(test_file)
    test_case = args.test_case
    temp_dir = args.temp_dir
    swift_syntax_test = args.swift_syntax_test
    swiftsyntax_lit_test_helper = args.swiftsyntax_lit_test_helper
    serialization_format = args.serialization_format

    if not os.path.exists(temp_dir):
        os.makedirs(temp_dir)

    # FIXME: This check is transitional, once SwiftSyntax starts using the
    # parser library it will become self-contained and not need
    # swift-syntax-test for its lit tests. Contents of testWithParserLib()
    # function will move here and replace what comes below.
    if not swift_syntax_test:
        pre_edit_file = temp_dir + '/' + test_file_name + '.' + test_case + \
            '.pre.swift'
        post_edit_file = temp_dir + '/' + test_file_name + '.' + test_case + \
            '.post.swift'
        after_roundtrip_file = temp_dir + '/' + test_file_name + '.' \
            + test_case + '.post_after_roundtrip.swift'
        return testWithParserLib(
            test_file=test_file,
            test_case=test_case,
            pre_edit_file=pre_edit_file,
            post_edit_file=post_edit_file,
            after_roundtrip_file=after_roundtrip_file,
            swiftsyntax_lit_test_helper=swiftsyntax_lit_test_helper)

    treeFileExtension = serialization_format

    pre_edit_tree_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.pre.' + treeFileExtension
    incremental_tree_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.incr.' + treeFileExtension
    post_edit_source_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.post.swift'
    after_roundtrip_source_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.post_after_roundtrip.swift'

    # Generate the syntax tree once incrementally and once from scratch
    try:
        serializeIncrParseMarkupFile(test_file=test_file,
                                     test_case=test_case,
                                     mode='pre-edit',
                                     serialization_mode='full',
                                     serialization_format=serialization_format,
                                     omit_node_ids=False,
                                     output_file=pre_edit_tree_file,
                                     diags_output_file=None,
                                     temp_dir=temp_dir,
                                     swift_syntax_test=swift_syntax_test,
                                     print_visual_reuse_info=False)

        serializeIncrParseMarkupFile(test_file=test_file,
                                     test_case=test_case,
                                     mode='incremental',
                                     serialization_mode='incremental',
                                     serialization_format=serialization_format,
                                     omit_node_ids=False,
                                     output_file=incremental_tree_file,
                                     diags_output_file=None,
                                     temp_dir=temp_dir,
                                     swift_syntax_test=swift_syntax_test,
                                     print_visual_reuse_info=False)
    except TestFailedError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print(e.message, file=sys.stderr)
        sys.exit(1)

    try:
        run_command([swiftsyntax_lit_test_helper, '-deserialize-incremental'] +
                    ['-serialization-format', serialization_format] +
                    ['-pre-edit-tree', pre_edit_tree_file] +
                    ['-incr-tree', incremental_tree_file] +
                    ['-out', after_roundtrip_source_file])
    except subprocess.CalledProcessError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print('Deserializing the swift file failed:\n', file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)

    # Check if the two syntax trees are the same
    try:
        run_command(
            [
                'diff', '-u',
                post_edit_source_file,
                after_roundtrip_source_file
            ])
    except subprocess.CalledProcessError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print('Source file after incrementally transferring the syntax tree '
              'to swiftSyntax does not match post-edit source file:\n\n',
              file=sys.stderr)
        print(e.output, file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
