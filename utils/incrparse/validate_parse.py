#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import sys

from test_util import TestFailedError, serializeIncrParseMarkupFile


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
        '--swift-syntax-test', required=True,
        help='The path to swift-syntax-test')
    parser.add_argument(
        '--print-visual-reuse-info', default=False, action='store_true',
        help='Print visual reuse information about the incremental parse \
              instead of diffing the syntax trees. This option is intended \
              for debug purposes only.')

    args = parser.parse_args(sys.argv[1:])

    test_file = args.file.name
    test_file_name = os.path.basename(test_file)
    test_case = args.test_case
    temp_dir = args.temp_dir
    swift_syntax_test = args.swift_syntax_test
    visual_reuse_info = args.print_visual_reuse_info

    if not os.path.exists(temp_dir):
        os.makedirs(temp_dir)

    incremental_serialized_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.postViaIncr.json'
    post_edit_serialized_file = temp_dir + '/' + test_file_name + '.' \
        + test_case + '.post.json'

    # Generate the syntax tree once incrementally and once from scratch
    try:
        serializeIncrParseMarkupFile(test_file=test_file,
                                     test_case=test_case,
                                     mode='incremental',
                                     serialization_mode='full',
                                     serialization_format='json',
                                     omit_node_ids=True,
                                     output_file=incremental_serialized_file,
                                     temp_dir=temp_dir + '/temp',
                                     swift_syntax_test=swift_syntax_test,
                                     print_visual_reuse_info=visual_reuse_info)
        if visual_reuse_info:
            # If we just want the reuse info, we don't need to parse the file
            # from scratch or validate it
            sys.exit(0)

        serializeIncrParseMarkupFile(test_file=test_file,
                                     test_case=test_case,
                                     mode='post-edit',
                                     serialization_mode='full',
                                     serialization_format='json',
                                     omit_node_ids=True,
                                     output_file=post_edit_serialized_file,
                                     temp_dir=temp_dir + '/temp',
                                     swift_syntax_test=swift_syntax_test,
                                     print_visual_reuse_info=visual_reuse_info)
    except TestFailedError as e:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print(e.message, file=sys.stderr)
        sys.exit(1)

    # Check if the two syntax trees are the same
    import difflib
    lines = difflib.unified_diff(open(incremental_serialized_file).readlines(),
                                 open(post_edit_serialized_file).readlines(),
                                 fromfile=incremental_serialized_file,
                                 tofile=incremental_serialized_file)
    diff = '\n'.join(line for line in lines)
    if diff:
        print('Test case "%s" of %s FAILed' % (test_case, test_file),
              file=sys.stderr)
        print('Syntax tree of incremental parsing does not match '
              'from-scratch parsing of post-edit file:\n\n', file=sys.stderr)
        print(diff, file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
