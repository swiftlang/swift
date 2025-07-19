#!/usr/bin/env python3

# This source file is part of the Swift.org open source project
#
# Copyright (c) 2024 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Utility to download unpublished toolchains
"""

import argparse
import os
import re
import sys
import tarfile
import urllib.request


_CI_URL_ = "https://ci.swift.org/job/oss-swift-package-"


def get_build_url(platform, arch):
    if arch == "x86_64" or arch == "Universal":
        arch = ""
    else:
        arch = f"-{arch}"
    return f"{_CI_URL_}{platform}{arch}/lastSuccessfulBuild/consoleText"


def get_latest_toolchain_url(build_url):
    toolchain_url = None
    with urllib.request.urlopen(build_url) as response:
        body = response.read().decode('utf-8')
    lines = body.split('\n')
    toolchain = [x for x in lines if 'Toolchain:' in x]
    if len(toolchain) > 0:
        toolchain_url = re.sub(r'.+Toolchain: (.+)$', r'\1', toolchain[0])
    return toolchain_url


def download_toolchain(toolchain_url, output_dir):
    file_name = toolchain_url.rsplit('/', 1)[-1]
    output_path = os.path.join(output_dir, file_name)
    urllib.request.urlretrieve(toolchain_url, output_path)
    return output_path


def untar_toolchain(output_dir, tar_path):
    with tarfile.open(tar_path, "r") as tf:
        tf.extractall(path=output_dir)


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--output-dir",
        required=True,
        help="Output dir to download the toolchain",
    )

    parser.add_argument(
        "--platform",
        required=True,
        choices=["amazon-linux-2",
                 "debian-12",
                 "fedora-39",
                 "macos",
                 "ubi-9",
                 "ubuntu-20_04",
                 "ubuntu-22_04",
                 "ubuntu-24_04"],
        help="Platform",
    )

    parser.add_argument(
        "--arch",
        required=True,
        choices=["x86_64", "aarch64", "Universal"],
        help="architectures",
    )

    parser.add_argument(
        "--untar",
        action=argparse.BooleanOptionalAction,
        help="Untar the toolchain in the output directory",
    )

    parser.add_argument(
        "--dry-run",
        default=False,
        action='store_true',
        help="Infer the toolchain URL but don't download it")

    return parser.parse_args()


def main():
    args = parse_args()

    if not os.path.isdir(args.output_dir):
        print(f"Error: Directory does not exist at {args.output_dir}.")
        return -1

    build_url = get_build_url(args.platform, args.arch)
    toolchain_url = get_latest_toolchain_url(build_url)
    if not toolchain_url:
        print(f"Error: Unable to find toolchain for {args.platform}.")
        return -1
    print(f"[Toolchain URL] {toolchain_url}")

    if not args.dry_run:
        output_path = download_toolchain(toolchain_url, args.output_dir)
        print(f"[Toolchain] {output_path}")

        if args.untar:
            untar_toolchain(args.output_dir, output_path)
            print(f"[Extracted] {args.output_dir}/usr")


if __name__ == "__main__":
    sys.exit(main())
