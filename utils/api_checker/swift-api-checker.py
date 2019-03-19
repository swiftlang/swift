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


def check_call(cmd, cwd=None, env=os.environ, verbose=True, output=None):
    if verbose:
        print(' '.join([escapeCmdArg(arg) for arg in cmd]))
    return subprocess.check_call(cmd, cwd=cwd, env=env,
                                 stderr=subprocess.STDOUT, stdout=output)


def check_output(cmd, verbose=False):
    if verbose:
        print(' '.join([escapeCmdArg(arg) for arg in cmd]))
    return subprocess.check_output(cmd).strip()


def get_sdk_path(platform):
    return check_output(['xcrun', '-sdk', platform, '-show-sdk-path'])


def prepare_module_list(platform, file):
    check_call([INFER_IMPORT_PATH, '-s', get_sdk_path(platform)], output=file)
    with open(INFER_IMPORT_DIR + '/fixed-modules-common.txt', 'r') as extra:
        file.write(extra.read())
    with open(INFER_IMPORT_DIR + '/fixed-modules-' + platform + '.txt',
              'r') as extra:
        file.write(extra.read())


class DumpConfig:
    def __init__(self, platform):
        target_map = {
            'iphoneos': 'arm64-apple-ios10.0',
            'macosx': 'x86_64-apple-macosx10.11',
            'appletvos': 'arm64-apple-tvos10.0',
            'watchos': 'armv7k-apple-watchos3.0',
        }
        self.platform = platform
        self.target = target_map[platform]
        self.sdk = get_sdk_path(platform)
        self.frameworks = [
            self.sdk + '/System/Library/Frameworks/',
            os.path.realpath(self.sdk + '/../../Library/Frameworks/')]
        self.tool_path = check_output(['xcrun', '--find',
                                       'swift-api-digester'])

    def run(self, output, module, swift_ver, abi):
        cmd = [self.tool_path, '-o', output, '-sdk', self.sdk, '-target',
               self.target, '-dump-sdk', '-module-cache-path',
               '/tmp/ModuleCache', '-swift-version',
               swift_ver, '-abort-on-module-fail']
        for path in self.frameworks:
            cmd.extend(['-iframework', path])
        if abi:
            cmd.extend(['-abi'])
        if module:
            cmd.extend(['-module', module])
            check_call(cmd)
        else:
            with tempfile.NamedTemporaryFile() as tmp:
                prepare_module_list(self.platform, tmp)
                cmd.extend(['-module-list-file', tmp.name])
                check_call(cmd)


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description='''
A convenient wrapper for swift-api-digester.
''')

    basic_group = parser.add_argument_group('Basic')
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

    basic_group.add_argument('--abi',
                             action='store_true',
                             help='Whether we are jsonizing for abi')

    args = parser.parse_args(sys.argv[1:])
    if not args.target:
        fatal_error("Need to specify --target")
    if not args.output:
        fatal_error("Need to specify --output")

    if args.action == 'dump':
        runner = DumpConfig(platform=args.target)
        runner.run(output=args.output, module=args.module,
                   swift_ver=args.swift_version, abi=args.abi)
    elif args.action == 'diagnose':
        fatal_error('Not implemented')
    else:
        fatal_error('Cannot recognize action: ' + args.action)


if __name__ == '__main__':
    main()
