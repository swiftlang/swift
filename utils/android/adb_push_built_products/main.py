# main.py - Push libraries to an Android device -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Android tests require certain libraries be available on the device. This
# script is a convenient way to deploy those libraries.
#
# ----------------------------------------------------------------------------

import argparse
import glob
import os
import sys

import adb.commands


def argument_parser():
    """Return an argument parser for this script."""
    parser = argparse.ArgumentParser(
        description='Convenience script for pushing Swift build products to '
                    'an Android device.')
    parser.add_argument(
        'paths',
        nargs='+',
        help='One or more paths to build products that should be pushed to '
             'the device. If you specify a directory, all files in the '
             'directory that end in ".so" will be pushed to the device.')
    parser.add_argument(
        '-d', '--destination',
        help='The directory on the device the files will be pushed to.',
        default=adb.commands.DEVICE_TEMP_DIR)
    parser.add_argument(
        '-n', '--ndk',
        help='The path to an Android NDK. If specified, the libc++ library '
             'in that NDK will be pushed to the device.',
        default=os.getenv('ANDROID_NDK_HOME', None))
    parser.add_argument(
        '-a', '--destination-arch',
        help='The architecture of the host device. Used to determine the '
             'right library versions to send to the device.',
        choices=['armv7', 'aarch64', 'x86_64'],
        default='armv7')
    return parser


def _push(sources, destination):
    print('Pushing "{}" to device path "{}".'.format(sources, destination))
    print(adb.commands.push(sources, destination))


def _find_libcpp(ndk_path, target_arch):
    ndk_version = 26
    # TODO: Add argument and keep support?
    if ndk_version < 22:
        return os.path.join(ndk_path,
                                'sources',
                                'cxx-stl',
                                'llvm-libc++',
                                'libs',
                                {
                                    'armv7': 'armeabi-v7a',
                                    'aarch64': 'arm64-v8a',
                                    'x86_64': 'x86_64'
                                }[target_arch],
                                'libc++_shared.so')
    # TODO: Detect host arch
    host_triple = 'windows-x86_64' if sys.platform == 'win32' else 'linux-x86_64'
    target_triple = target_arch + '-linux-android'

    # TODO: Support just-built libc++?
    return os.path.join(ndk_path, 'toolchains', 'llvm', 'prebuilt', host_triple, 'sysroot', 'usr', 'lib', target_triple, 'libc++_shared.so')


def main():
    """
    The main entry point for adb_push_built_products.

    Parse arguments and kick off the script. Return zero to indicate success.
    Raises an exception otherwise.
    """
    parser = argument_parser()
    args = parser.parse_args()

    for path in args.paths:
        adb.commands.shell(['mkdir', '-p', args.destination])
        if os.path.isdir(path):
            full_paths = [
                os.path.join(path, basename)
                for basename in glob.glob(os.path.join(path, '*.so'))]
            _push(full_paths, args.destination)
        else:
            _push(path, args.destination)

    if args.ndk:
        libcpp = _find_libcpp(args.ndk, args.destination_arch)
        _push(libcpp, args.destination)

    return 0
