#!/usr/bin/env python

# This tool helps assess the impact of automatically applying
# heuristics that omit 'needless' words from APIs imported from Clang
# into Swift.

from __future__ import print_function

import argparse
import os
import re
import subprocess

DEFAULT_TARGET_BASED_ON_SDK = {
    'macosx': 'x86_64-apple-macosx10.11',
    'iphoneos': 'arm64-apple-ios9.0',
    'iphonesimulator': 'x86_64-apple-ios9.0',
    'watchos': 'armv7k-apple-watchos2.0',
    'watchos.simulator': 'i386-apple-watchos2.0',
    'appletvos': 'arm64-apple-tvos9',
    'appletvos.simulator': 'x86_64-apple-tvos9',
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
    parser.add_argument('-3', '--swift-3', action='store_true', help="Use Swift 3 transformation")
    parser.add_argument('-o', '--output-dir', default=os.getcwd(), help='Directory to which the output will be emitted.')
    parser.add_argument('-q', '--quiet', action='store_true', help='Suppress printing of status messages.')
    return parser

def output_command_result_to_file(command_args, filename):
    with open(filename, 'w') as output_file:
        subprocess.call(command_args, stdout=output_file)

def run_command(args):
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    exitcode = proc.returncode
    return (exitcode, out, err)

# Collect the set of submodules for the given module.
def collect_submodules(common_args, module):
    # Execute swift-ide-test to print the interface.
    my_args = ['-module-print-submodules', '-module-to-print=%s' % (module)]
    (exitcode, out, err) = run_command(common_args + my_args)
    if exitcode != 0:
        print('error: submodule collection failed with error %d' % (exitcode))
        return ()

    # Find all of the submodule imports.
    import_matcher = re.compile('.*import\s+%s\.([A-Za-z_0-9.]+)' % (module))
    submodules = set()
    for line in out.splitlines():
        match = import_matcher.match(line)
        if match:
            submodules.add(match.group(1))

    return sorted(list(submodules))

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

    swift_ide_test_cmd_common = [args.swift_ide_test, '-print-module', '-source-filename', source_filename, '-sdk', sdkroot, '-target', args.target, '-module-print-skip-overlay', '-skip-unavailable', '-skip-print-doc-comments']

    submodules = collect_submodules(swift_ide_test_cmd_common, args.module)

    # Determine the set of arguments we'll use.
    swift_ide_test_cmd = swift_ide_test_cmd_common + ['-skip-imports']
    if args.swift_3:
        swift_ide_test_cmd = swift_ide_test_cmd + ['-enable-omit-needless-words', '-enable-infer-default-arguments']

    # Create a .swift file we can feed into swift-ide-test
    subprocess.call(['touch', source_filename])

    # Dump the top-level module
    output_dir = args.output_dir
    subprocess.call(['mkdir', '-p', ('%s/%s' % (output_dir, args.module))])
    output_file = '%s/%s/%s.swift' % (output_dir, args.module, args.module)
    if not args.quiet:
        print('Writing %s...' % output_file)

    cmd = swift_ide_test_cmd + ['-module-to-print=%s' % (args.module)]
    output_command_result_to_file(cmd, output_file)

    # Dump each submodule.
    for submodule in submodules:
        output_file = '%s/%s/%s.swift' % (output_dir, args.module, submodule)
        if not args.quiet:
            print('Writing %s...' % output_file)

        full_submodule = '%s.%s' % (args.module, submodule)
        cmd = swift_ide_test_cmd + ['-module-to-print=%s' % (full_submodule)]
        output_command_result_to_file(cmd, output_file)

    # Remove the .swift file we fed into swift-ide-test
    subprocess.call(['rm', '-f', source_filename])

if __name__ == '__main__':
    main()
