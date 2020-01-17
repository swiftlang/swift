# utils/SwiftBuildSupport.py - Utilities for Swift build scripts -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import print_function

import os


HOME = os.environ.get("HOME", "/")


def _get_default_source_root():
    result = ""

    # Are we in a Swift checkout? Start from this file and check its parent
    # directories.
    #
    # $SWIFT_SOURCE_ROOT/swift/utils/swift_build_support/swift_build_support/SwiftBuildSupport.py
    utils_path = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
    (swift_path, parent_dirname) = os.path.split(utils_path)
    if parent_dirname != "utils":
        return result
    if not os.path.exists(os.path.join(swift_path, 'CMakeLists.txt')):
        return result
    result = os.path.dirname(swift_path)

    # Are we in an LLVM checkout? Start from the Swift checkout and check /its/
    # parent directories.
    #
    # $SWIFT_SOURCE_ROOT/llvm/tools/swift/utils/swift_build_support/swift_build_support/SwiftBuildSupport.py
    (llvm_path, parent_dirname) = os.path.split(result)
    if parent_dirname != "tools":
        return result
    if not os.path.exists(os.path.join(llvm_path, 'CMakeLists.txt')):
        return result
    result = os.path.dirname(llvm_path)

    return result


# Set SWIFT_SOURCE_ROOT in your environment to control where the sources
# are found.
SWIFT_SOURCE_ROOT = os.environ.get(
    "SWIFT_SOURCE_ROOT", _get_default_source_root())

# Set SWIFT_BUILD_ROOT to a directory that will contain a subdirectory
# for each build configuration
SWIFT_BUILD_ROOT = os.environ.get(
    "SWIFT_BUILD_ROOT", os.path.join(SWIFT_SOURCE_ROOT, "build"))


def _get_default_swift_repo_name():
    result = ""

    # Are we in a Swift checkout? Start from this file and check its parent
    # directories.
    #
    # $SWIFT_SOURCE_ROOT/$SWIFT_REPO_NAME/utils/swift_build_support/swift_build_support/SwiftBuildSupport.py
    utils_path = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
    (swift_path, parent_dirname) = os.path.split(utils_path)
    if parent_dirname != "utils":
        return result
    if not os.path.exists(os.path.join(swift_path, 'CMakeLists.txt')):
        return result
    (_, swift_repo_name) = os.path.split(swift_path)
    return swift_repo_name


# Set SWIFT_REPO_NAME in your environment to control the name of the swift
# directory name that is used.
SWIFT_REPO_NAME = os.environ.get(
    "SWIFT_REPO_NAME", _get_default_swift_repo_name())
