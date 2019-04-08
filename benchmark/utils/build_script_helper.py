#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--verbose', '-v', action='store_true')
    parser.add_argument('--package-path', type=str, required=True)
    parser.add_argument('--build-path', type=str, required=True)
    parser.add_argument('--toolchain', type=str, required=True)

    # Build the debug/release versions.
    args = parser.parse_args()
    swiftbuild_path = os.path.join(args.toolchain, 'usr', 'bin', 'swift-build')
    swiftbuild_args = [
        swiftbuild_path,
        '--package-path', args.package_path,
        '--build-path', args.build_path,
        '--configuration', 'debug',
    ]
    if args.verbose:
        swiftbuild_args.append('--verbose')
    subprocess.call(swiftbuild_args)

    swiftbuild_args = [
        swiftbuild_path,
        '--package-path', args.package_path,
        '--build-path', args.build_path,
        '--configuration', 'release',
        '-Xswiftc', '-Xllvm',
        '-Xswiftc', '-align-module-to-page-size',
    ]
    if args.verbose:
        swiftbuild_args.append('--verbose')
    subprocess.call(swiftbuild_args)


if __name__ == "__main__":
    main()
