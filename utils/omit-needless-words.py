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

# Dump the API for the given module.
def dump_module_api(cmd, extra_dump_args, output_dir, module, quiet):
    # Collect the submodules
    submodules = collect_submodules(cmd, module)

    # Dump the top-level module
    subprocess.call(['mkdir', '-p', ('%s/%s' % (output_dir, module))])
    output_file = '%s/%s/%s.swift' % (output_dir, module, module)
    if not quiet:
        print('Writing %s...' % output_file)

    top_level_cmd = cmd + extra_dump_args + ['-module-to-print=%s' % (module)]
    output_command_result_to_file(top_level_cmd, output_file)

    # Dump each submodule.
    for submodule in submodules:
        output_file = '%s/%s/%s.swift' % (output_dir, module, submodule)
        if not quiet:
            print('Writing %s...' % output_file)

        full_submodule = '%s.%s' % (module, submodule)
        submodule_cmd = cmd + extra_dump_args
        submodule_cmd = submodule_cmd + ['-module-to-print=%s' % (full_submodule)]
        output_command_result_to_file(submodule_cmd, output_file)

    return

# Collect the set of frameworks we should dump 
def collect_frameworks(sdk):
    (exitcode, out, err) = run_command(["xcrun", "--show-sdk-path", "-sdk", sdk])
    if exitcode != 0:
        print('error: framework collection failed with error %d' % (exitcode))
        return ()
    sdk_path = out
    
    (exitcode, out, err) = run_command(["xcrun", "--show-sdk-version", "-sdk", sdk])
    if exitcode != 0:
        print('error: framework collection failed with error %d' % (exitcode))
        return ()



        
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

    # Determine the set of extra arguments we'll use.
    extra_args = ['-skip-imports']
    if args.swift_3:
        extra_args = extra_args + ['-enable-omit-needless-words', '-enable-infer-default-arguments', '-enable-strip-ns-prefix']

    # Create a .swift file we can feed into swift-ide-test
    subprocess.call(['touch', source_filename])

    # Dump the API for this module.
    dump_module_api(swift_ide_test_cmd_common, extra_args, args.output_dir,
                    args.module, args.quiet)

    # Remove the .swift file we fed into swift-ide-test
    subprocess.call(['rm', '-f', source_filename])

if __name__ == '__main__':
    main()
