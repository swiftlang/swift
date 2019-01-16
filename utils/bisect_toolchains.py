#!/usr/bin/env python
#
# utils/bisect_toolchains.py - Bisect swift.org toolchains with tags
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import requests
import subprocess
import sys
import os


SWIFT_BASE_URL = 'https://swift.org/builds'
GITHUB_BASE_URL = 'https://api.github.com'


def download_toolchain(platform, tag, branch, workspace):
   swift_path = None
   if platform == 'osx':
      file_type = 'pkg'
      toolchain_type = 'xcode'
   else:
      file_type = 'tar.gz'
      toolchains_type = platform

   if branch != 'development':
      branch = 'swift-{branch}-branch'.format(branch=branch)

   download_url = "{base_url}/{branch}/{toolchain_type}/{tag}/{tag}-{platform}.{file_type}".format(
      base_url=SWIFT_BASE_URL,
      branch=branch,
      toolchain_type=toolchain_type,
      tag=tag,
      platform=platform,
      file_type=file_type)

   print("Downloading: {url}".format(url=download_url))
   r = requests.get(download_url, allow_redirects=True)
   download_path="{workspace}/{tag}-{platform}.{file_type}".format(
      workspace=workspace,
      tag=tag,
      platform=platform,
      file_type=file_type)

   with open(download_path, 'wb') as f:
      f.write(r.content)

   if platform == 'osx':
      print("Installing: {download_path}".format(download_path=download_path))
      toolchain_dir = "{workspace}/{tag}-{platform}".format(
         download_path=download_path, workspace=workspace, tag=tag, platform=platform)
      subprocess.call("pkgutil --expand {download_path} {toolchain_dir}".format(
         download_path=download_path,
         toolchain_dir=toolchain_dir), shell=True)
      payload_path = "{toolchain_dir}/{tag}-osx-package.pkg/Payload".format(
         toolchain_dir=toolchain_dir,
         tag=tag)
      subprocess.call("tar xf {payload_path} -C {toolchain_dir}".format(
         payload_path=payload_path,
         toolchain_dir=toolchain_dir), shell=True)
      swift_path = "{toolchain_dir}/usr/bin/swift".format(
         toolchain_dir=toolchain_dir)
      subprocess.call("{swift_path} --version".format(
         swift_path=swift_path), shell=True)
   return swift_path

def get_tags(branch):
   tags = []
   tag_startswith = 'swift-{branch}'.format(branch=branch.upper())
   tags_url = "{base_url}/repos/apple/swift/git/refs/tags".format(
    base_url=GITHUB_BASE_URL)
   r = requests.get(tags_url, allow_redirects=True)
   tags_json = r.json()

   for tag in tags_json:
      tag = tag['ref'].replace('refs/tags/','')
      if tag.startswith(tag_startswith):
         tags.append(tag)
   return sorted(tags, reverse=True)

def main():

   parser = argparse.ArgumentParser(
      formatter_class=argparse.RawDescriptionHelpFormatter,
      description="""
      Swift Bisect Swift.org Toolchains.""")
   parser.add_argument(
      "--platform",
      help="OS platform to test with",
      choices=['osx', 'ubuntu1404', 'ubuntu1604', 'ubuntu1804'],
      default='osx',
      required=True)
   parser.add_argument(
      "--branch",
      help="OS platform to test with",
      choices=['development', '5.0'],
      default='development',
      required=True)
   parser.add_argument(
      "--workspace",
      help="Directory to toolchains download",
      required=True)
   parser.add_argument(
      "--good-tag",
      help="Good tag (Oldest tag)",
      required=True)
   parser.add_argument(
      "--bad-tag",
      help="Bad tag (Newest tag)",
      required=True)

   args = parser.parse_args()

   if args.platform != "osx":
      print("{platform} is currently not supported by Swift bisect tool.".format(platform=args.platform))
   else:
      platform = args.platform

   good_tag = args.good_tag
   bad_tag = args.bad_tag
   workspace = args.workspace
   branch = args.branch

   # Setup workspace directory
   if not os.path.exists(workspace):
      print("Creating {workspace}".format(workspace=workspace))
      os.makedirs(workspace)

   tags = get_tags(branch)
   start_index = tags.index(bad_tag)
   stop_index = tags.index(good_tag)

   print("Testing {count} toolchains:".format(count=stop_index - start_index))
   
   index = start_index
   while(index <= stop_index):
      download_toolchain(platform, tags[index], branch, workspace)
      index = index + 1

if __name__ == "__main__":
   try:
      sys.exit(main())
   except KeyboardInterrupt:
      sys.exit(1)
