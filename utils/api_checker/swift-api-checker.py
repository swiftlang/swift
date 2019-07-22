#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import subprocess
import sys
import tempfile

INFER_IMPORT_DIR = \
    os.path.dirname(os.path.realpath(__file__)) + '/sdk-module-lists'

INFER_IMPORT_PATH = INFER_IMPORT_DIR + '/infer-imports.py'


def printerr(message):
    print(message, file=sys.stderr)


def fatal_error(message):
    printerr(message)
    sys.exit(1)


def escapeCmdArg(arg):
    if '"' in arg or ' ' in arg:
        return '"%s"' % arg.replace('"', '\\"')
    else:
        return arg


def check_call(cmd, cwd=None, env=os.environ, verbose=False, output=None):
    if verbose:
        print(' '.join([escapeCmdArg(arg) for arg in cmd]))
    return subprocess.check_call(cmd, cwd=cwd, env=env,
                                 stderr=None, stdout=output)


def check_output(cmd, verbose=False):
    if verbose:
        print(' '.join([escapeCmdArg(arg) for arg in cmd]))
    return subprocess.check_output(cmd).strip()


def get_sdk_path(platform):
    return check_output(['xcrun', '-sdk', platform, '-show-sdk-path'])


def prepare_module_list(platform, file, verbose, module_filter_flags,
                        include_fixed_modules):
    cmd = [INFER_IMPORT_PATH, '-s', get_sdk_path(platform)]
    cmd.extend(module_filter_flags)
    if verbose:
        cmd.extend(['--v'])
    check_call(cmd, output=file)
    # The fixed modules are all objc frameworks.
    if not include_fixed_modules:
        return
    with open(INFER_IMPORT_DIR + '/fixed-modules-common.txt', 'r') as extra:
        file.write(extra.read())
    with open(INFER_IMPORT_DIR + '/fixed-modules-' + platform + '.txt',
              'r') as extra:
        file.write(extra.read())


def get_api_digester_path(tool_path):
    if tool_path:
        return tool_path
    return check_output(['xcrun', '--find', 'swift-api-digester'])


def create_directory(path):
    if not os.path.isdir(path):
        os.makedirs(path)


class DumpConfig:
    def __init__(self, tool_path, platform):
        target_map = {
            'iphoneos': 'arm64-apple-ios10.0',
            'macosx': 'x86_64-apple-macosx10.11',
            'appletvos': 'arm64-apple-tvos10.0',
            'watchos': 'armv7k-apple-watchos3.0',
        }
        self.tool_path = get_api_digester_path(tool_path)
        self.platform = platform
        self.target = target_map[platform]
        self.sdk = get_sdk_path(platform)
        self.frameworks = [
            self.sdk + '/System/Library/Frameworks/',
            os.path.realpath(self.sdk + '/../../Library/Frameworks/')]

    def run(self, output, module, swift_ver, opts, verbose,
            module_filter_flags, include_fixed_modules, separate_by_module):
        cmd = [self.tool_path, '-sdk', self.sdk, '-target',
               self.target, '-dump-sdk', '-module-cache-path',
               '/tmp/ModuleCache', '-swift-version',
               swift_ver, '-abort-on-module-fail']
        for path in self.frameworks:
            cmd.extend(['-iframework', path])
        cmd.extend(['-' + o for o in opts])
        if verbose:
            cmd.extend(['-v'])
        if module:
            cmd.extend(['-module', module])
            cmd.extend(['-o', output])
            check_call(cmd, verbose=verbose)
        else:
            with tempfile.NamedTemporaryFile() as tmp:
                prepare_module_list(self.platform, tmp, verbose,
                                    module_filter_flags, include_fixed_modules)
                if separate_by_module:
                    tmp.seek(0)
                    create_directory(output)
                    for module in [name.strip() for name in tmp.readlines()]:
                        dir_path = os.path.realpath(output + '/' + module)
                        file_path = os.path.realpath(dir_path + '/' +
                                                     self.platform + '.json')
                        create_directory(dir_path)
                        current_cmd = list(cmd)
                        current_cmd.extend(['-module', module])
                        current_cmd.extend(['-o', file_path])
                        check_call(current_cmd, verbose=verbose)
                else:
                    cmd.extend(['-o', output])
                    cmd.extend(['-module-list-file', tmp.name])
                    check_call(cmd, verbose=verbose)


class DiagnoseConfig:
    def __init__(self, tool_path):
        self.tool_path = get_api_digester_path(tool_path)

    def run(self, opts, before, after, output, verbose):
        cmd = [self.tool_path, '-diagnose-sdk', '-input-paths', before,
               '-input-paths', after, '-print-module']
        if output:
            cmd.extend(['-o', output])
        cmd.extend(['-' + o for o in opts])
        if verbose:
            cmd.extend(['-v'])
        check_call(cmd, verbose=verbose)


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='''
A convenient wrapper for swift-api-digester.
''')

    basic_group = parser.add_argument_group('Basic')

    basic_group.add_argument('--tool-path', default=None, help='''
        the path to a swift-api-digester; if not specified, the script will
        use the one from the toolchain
        ''')

    basic_group.add_argument('--action', default='', help='''
        the action to perform for swift-api-digester
        ''')

    basic_group.add_argument('--target', default=None, help='''
        one of macosx, iphoneos, appletvos, and watchos
        ''')

    basic_group.add_argument('--output', default=None, help='''
        the output file of the module baseline should end with .json
        ''')

    basic_group.add_argument('--swift-version', default='4', help='''
        Swift version to use; default is 4
        ''')

    basic_group.add_argument('--module', default=None, help='''
        name of the module/framework to generate baseline, e.g. Foundation
        ''')

    basic_group.add_argument('--module-filter', default='', help='''
        the action to perform for swift-api-digester
        ''')

    basic_group.add_argument('--opts', nargs='+', default=[], help='''
        additional flags to pass to swift-api-digester
        ''')

    basic_group.add_argument('--v',
                             action='store_true',
                             help='Process verbosely')

    basic_group.add_argument('--dump-before',
                             action=None,
                             help='''
        Path to the json file generated before change'
        ''')

    basic_group.add_argument('--dump-after',
                             action=None,
                             help='''
        Path to the json file generated after change
        ''')

    basic_group.add_argument('--separate-by-module',
                             action='store_true',
                             help='When importing entire SDK, dump content '
                                  'seprately by module names')
    args = parser.parse_args(sys.argv[1:])

    if args.action == 'dump':
        if not args.target:
            fatal_error("Need to specify --target")
        if not args.output:
            fatal_error("Need to specify --output")
        if args.module_filter == '':
            module_filter_flags = []
            include_fixed_modules = True
        elif args.module_filter == 'swift-frameworks-only':
            module_filter_flags = ['--swift-frameworks-only']
            include_fixed_modules = False
        elif args.module_filter == 'swift-overlay-only':
            module_filter_flags = ['--swift-overlay-only']
            include_fixed_modules = False
        else:
            fatal_error("cannot recognize --module-filter")
        runner = DumpConfig(tool_path=args.tool_path, platform=args.target)
        runner.run(output=args.output, module=args.module,
                   swift_ver=args.swift_version, opts=args.opts,
                   verbose=args.v,
                   module_filter_flags=module_filter_flags,
                   include_fixed_modules=include_fixed_modules,
                   separate_by_module=args.separate_by_module)
    elif args.action == 'diagnose':
        if not args.dump_before:
            fatal_error("Need to specify --dump-before")
        if not args.dump_after:
            fatal_error("Need to specify --dump-after")
        runner = DiagnoseConfig(tool_path=args.tool_path)
        runner.run(opts=args.opts, before=args.dump_before,
                   after=args.dump_after, output=args.output, verbose=args.v)
    else:
        fatal_error('Cannot recognize action: ' + args.action)


if __name__ == '__main__':
    main()
