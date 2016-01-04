#!/usr/bin/env python

# This tool helps assess the impact of automatically applying
# heuristics that omit 'needless' words from APIs imported from Clang
# into Swift.

from __future__ import print_function

import argparse
import subprocess

DEFAULT_TARGET_BASED_ON_SDK = {
    'macosx'              : 'x86_64-apple-macosx10.11',
    'iphoneos'            : 'arm64-apple-ios9.0',
    'iphonesimulator'     : 'x86_64-apple-ios9.0',
    'watchos'             : 'armv7k-apple-watchos2.0',
    'watchos.simulator'   : 'i386-apple-watchos2.0',
    'appletvos'           : 'arm64-apple-tvos9',
    'appletvos.simulator' : 'x86_64-apple-tvos9',
}

def create_parser():
    parser = argparse.ArgumentParser(
        description="Determines the effects of omitting 'needless' words from imported APIs",
        prog='omit-needless-words.py',
        usage='python omit-needless-words.py -m AppKit')
    parser.add_argument('-m', '--module', required=True, help='The module name.')
    parser.add_argument('-s', '--sdk', default='macosx', help="The SDK to use.")
    parser.add_argument('-t', '--target', help="The target triple to use.")
    parser.add_argument('-i', '--swift-ide-test', default='swift-ide-test', help="The swift-ide-test executable.")
    parser.add_argument('-d', '--diff_tool', default='opendiff', help="The tool to use to diff the results.")
    parser.add_argument('-b', '--only-before', action='store_true', help='Only emit the "before" result.')
    parser.add_argument('--before-file', help='Emit "before" results to the given file [defaults to <modulename>.before.txt].')
    parser.add_argument('-a', '--only-after', action='store_true', help='Only emit the "after" result.')
    parser.add_argument('--after-file', help='Emit "after" results to the given file [defaults to <modulename>.after.txt].')
    parser.add_argument('-q', '--quiet', action='store_true', help='Suppress printing of status messages.')
    return parser

def output_command_result_to_file(command_args, filename):
    with open(filename, 'w') as output_file:
        subprocess.call(command_args, stdout=output_file)

def main():
    source_filename = 'omit-needless-words.swift'
    parser = create_parser()
    args = parser.parse_args()
    if not args.target:
        args.target = DEFAULT_TARGET_BASED_ON_SDK[args.sdk]

    # Figure out the SDK root for the requested SDK
    sdkroot = subprocess.check_output(['xcrun', '--show-sdk-path', '--sdk', args.sdk]).rstrip()
    if not args.quiet:
        print('SDK Root = %s' % (sdkroot))

    swift_ide_test_cmd = [args.swift_ide_test, '-print-module', '-source-filename', source_filename, '-sdk', sdkroot, '-target', args.target, '-module-print-skip-overlay', '-skip-unavailable', '-module-print-submodules', '-skip-imports', '-module-to-print=%s' % (args.module)]
    omit_needless_words_args = ['-enable-omit-needless-words', '-enable-infer-default-arguments']

    # Determine the output files.
    # No good way with argparse to set default value based on dependency of other arg.
    if not args.before_file:
        args.before_file = '%s.before.txt' % (args.module)
    if not args.after_file:
        args.after_file = '%s.after.txt' % (args.module)

    # Create a .swift file we can feed into swift-ide-test
    subprocess.call(['touch', source_filename])

    if not args.only_after:
      # Print the interface without omitting needless words
      if not args.quiet:
          print('Writing %s...' % args.before_file)
      output_command_result_to_file(swift_ide_test_cmd, args.before_file)

    if not args.only_before:
      # Print the interface omitting needless words
      if not args.quiet:
          print('Writing %s...' % args.after_file)
      output_command_result_to_file(swift_ide_test_cmd + omit_needless_words_args, args.after_file)

    # Remove the .swift file we fed into swift-ide-test
    subprocess.call(['rm', '-f', source_filename])

    # Diff them
    if args.diff_tool != "":
        subprocess.call([args.diff_tool, args.before_file, args.after_file])

if __name__ == '__main__':
    main()

