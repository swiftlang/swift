# swift_build_support/workspace.py ------------------------------*- python -*-
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
"""
Represent whole source tree and the build directory
"""
# ----------------------------------------------------------------------------

import os.path


class Workspace(object):
    def __init__(self, source_root, build_root):
        self.source_root = source_root
        self.build_root = build_root

    def source_dir(self, path):
        return os.path.join(self.source_root, path)

    def build_dir(self, deployment_target, product):
        return os.path.join(self.build_root,
                            '%s-%s' % (product, deployment_target))


def compute_build_subdir(args):
    # Create a name for the build directory.
    build_subdir = args.cmake_generator.replace(" ", "_")

    cmark_build_dir_label = args.cmark_build_variant
    if args.cmark_assertions:
        cmark_build_dir_label += "Assert"

    llvm_build_dir_label = args.llvm_build_variant
    if args.llvm_assertions:
        llvm_build_dir_label += "Assert"

    swift_build_dir_label = args.swift_build_variant
    if args.swift_assertions:
        swift_build_dir_label += "Assert"
    if args.swift_analyze_code_coverage != "false":
        swift_build_dir_label += "Coverage"

    swift_stdlib_build_dir_label = args.swift_stdlib_build_variant
    if args.swift_stdlib_assertions:
        swift_stdlib_build_dir_label += "Assert"

    # FIXME: mangle LLDB build configuration into the directory name.
    if (llvm_build_dir_label == swift_build_dir_label and
            llvm_build_dir_label == swift_stdlib_build_dir_label and
            swift_build_dir_label == cmark_build_dir_label):
        # Use a simple directory name if all projects use the same build
        # type.
        build_subdir += "-" + llvm_build_dir_label
    elif (llvm_build_dir_label != swift_build_dir_label and
            llvm_build_dir_label == swift_stdlib_build_dir_label and
            swift_build_dir_label == cmark_build_dir_label):
        # Swift build type differs.
        build_subdir += "-" + llvm_build_dir_label
        build_subdir += "+swift-" + swift_build_dir_label
    elif (llvm_build_dir_label == swift_build_dir_label and
            llvm_build_dir_label != swift_stdlib_build_dir_label and
            swift_build_dir_label == cmark_build_dir_label):
        # Swift stdlib build type differs.
        build_subdir += "-" + llvm_build_dir_label
        build_subdir += "+stdlib-" + swift_stdlib_build_dir_label
    elif (llvm_build_dir_label == swift_build_dir_label and
            llvm_build_dir_label == swift_stdlib_build_dir_label and
            swift_build_dir_label != cmark_build_dir_label):
        # cmark build type differs.
        build_subdir += "-" + llvm_build_dir_label
        build_subdir += "+cmark-" + cmark_build_dir_label
    else:
        # We don't know how to create a short name, so just mangle in all
        # the information.
        build_subdir += "+cmark-" + cmark_build_dir_label
        build_subdir += "+llvm-" + llvm_build_dir_label
        build_subdir += "+swift-" + swift_build_dir_label
        build_subdir += "+stdlib-" + swift_stdlib_build_dir_label

    return build_subdir
