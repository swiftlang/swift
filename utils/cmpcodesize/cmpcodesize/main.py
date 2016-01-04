#!/usr/bin/env python
# cmpcodesize/main.py - Command-line entry point for cmpcodesize -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import print_function

import argparse
import sys
import os
import glob
import collections

from cmpcodesize.compare import \
    compareFunctionSizes, compareSizesOfFile, listFunctionSizes, readSizes


SHORTCUTS = {
    "O": "bin/PerfTests_O",
    "Ounchecked": "bin/PerfTests_Ounchecked",
    "Onone": "bin/PerfTests_Onone",
    "dylib": "lib/swift/macosx/x86_64/libswiftCore.dylib",
}


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""
Compares code sizes of "new" files, taking "old" files as a reference.

Environment variables:
    SWIFT_NEW_BUILDDIR   The old build-dir
E.g. $HOME/swift-work/build/Ninja-ReleaseAssert+stdlib-Release/swift-macosx-x86_64
    SWIFT_OLD_BUILDDIR   The new build-dir
E.g. $HOME/swift-reference/build/Ninja-ReleaseAssert+stdlib-Release/swift-macosx-x86_64

How to specify files:
1) No files:
    Compares codesize of the PerfTests_* executables and the swiftCore dylib in the new and old build-dirs.
    Example:
        cmpcodesize

2) One or more paths relative to the build-dirs (can be a pattern):
    Compares the files in the new and old build-dirs.
    Aliases:
        O          => bin/PerfTests_O
        Ounchecked => bin/PerfTests_Ounchecked
        Onone      => bin/PerfTests_Onone
        dylib      => lib/swift/macosx/x86_64/libswiftCore.dylib
    Examples:
        cmpcodesize Onone
        cmpcodesize benchmark/PerfTestSuite/O/*.o

3) Two files:
    Compares these two files (the first is the old file).
    Example:
        cmpcodesize test.o newversion.o

4) Two lists of files, separated by '--':
    Compares a set a files.
    Example:
        cmpcodesize olddir/*.o -- newdir/*.o

5) One file (only available with the -l option):
    Lists function sizes for that file
    Example:
        cmpcodesize -l test.o""")

    # Optional arguments.
    parser.add_argument('-a', '--additional-sections',
                        help='Show sizes of additional sections.',
                        action='store_true',
                        dest='all_sections',
                        default=False)
    parser.add_argument('-c', '--category',
                        help='Show functions by category.',
                        action='store_true',
                        dest='list_categories',
                        default=False)
    parser.add_argument('-l', '--list',
                        help='List all functions (can be a very long list). ' +
                             'Cannot be used in conjunction with ' +
                             '--additional-sections or --category. ' +
                             'You must specify between one and two files ' +
                             'when using this option.',
                        action='store_true',
                        dest='list_functions',
                        default=False)
    parser.add_argument('-s', '--summarize',
                        help='Summarize the sizes of multiple files instead ' +
                             'of listing each file separately.',
                        action='store_true',
                        dest='sum_sizes',
                        default=False)

    # Positional arguments.
    # These can be specified in means beyond what argparse supports,
    # so we gather them in a list and parse them manually.
    parser.add_argument('files', nargs='*',
                        help='A list of old and new files.')

    # argparse can't handle an '--' argument, so we replace it with
    # a custom identifier.
    separator_token = '*-*-*'
    parsed_arguments = parser.parse_args(
        [separator_token if arg == '--' else arg for arg in sys.argv[1:]])

    if parsed_arguments.list_functions:
        # --list is mutually exclusive with both --additional-sections
        # and --category. argparse is only capable of expressing mutual
        # exclusivity among options, not among groups of options, so
        # we detect this case manually.
        assert (not parsed_arguments.all_sections and
                not parsed_arguments.list_categories), \
            'Incorrect usage: --list cannot be specified in conjunction ' + \
            'with --additional-sections or --category.'
        # A file must be specified when using --list.
        assert parsed_arguments.files, \
            'Incorrect usage: Must specify between one and two files when ' + \
            'using --list, but you specified no files.'

    if separator_token in parsed_arguments.files:
        separator_index = parsed_arguments.files.index(separator_token)
        oldFiles = parsed_arguments.files[:separator_index]
        newFiles = parsed_arguments.files[separator_index + 1:]
    else:
        oldFileArgs = parsed_arguments.files

        oldBuildDir = os.environ.get("SWIFT_OLD_BUILDDIR")
        newBuildDir = os.environ.get("SWIFT_NEW_BUILDDIR")

        if not parsed_arguments.files:
            assert oldBuildDir and newBuildDir, \
                'Incorrect usage: You must specify either a list of ' + \
                'files, or have both $SWIFT_OLD_BUILDDIR and ' + \
                '$SWIFT_NEW_BUILDDIR environment variables set.\n' + \
                '$SWIFT_OLD_BUILDDIR = {0}\n$SWIFT_NEW_BUILDDIR = {1}'.format(
                    oldBuildDir, newBuildDir)
            oldFileArgs = list(SHORTCUTS.keys())

        oldFiles = []
        newFiles = []
        numExpanded = 0
        for file in oldFileArgs:
            if file in SHORTCUTS:
                file = SHORTCUTS[file]

            if not file.startswith("./") and oldBuildDir and newBuildDir:
                oldExpanded = glob.glob(os.path.join(oldBuildDir, file))
                newExpanded = glob.glob(os.path.join(newBuildDir, file))
                if oldExpanded and newExpanded:
                    oldFiles.extend(oldExpanded)
                    newFiles.extend(newExpanded)
                    numExpanded += 1

        if numExpanded != 0 and numExpanded != len(oldFileArgs):
            sys.exit("mix of expanded/not-expanded arguments")
        if numExpanded == 0:
            if len(oldFileArgs) > 2:
                sys.exit("too many arguments")
            oldFiles = oldFileArgs[0:1]
            newFiles = oldFileArgs[1:2]

    for file in (oldFiles + newFiles):
        if not os.path.isfile(file):
            sys.exit("file " + file + " not found")

    if parsed_arguments.list_functions:
        if not newFiles:
            sizes = collections.defaultdict(int)
            for file in oldFiles:
                readSizes(sizes, file, True, False)
            print(listFunctionSizes(sizes.items()))
        else:
            compareFunctionSizes(oldFiles, newFiles)
    else:
        print("%-26s%16s  %8s  %8s  %s" % ("", "Section", "Old", "New", "Percent"))
        if parsed_arguments.sum_sizes:
            compareSizesOfFile(oldFiles, newFiles,
                               parsed_arguments.all_sections,
                               parsed_arguments.list_categories)
        else:
            if len(oldFiles) != len(newFiles):
                sys.exit("number of new files must be the same of old files")

            oldFiles.sort
            newFiles.sort

            for idx, oldFile in enumerate(oldFiles):
                newFile = newFiles[idx]
                compareSizesOfFile([oldFile], [newFile],
                                   parsed_arguments.all_sections,
                                   parsed_arguments.list_categories)

if __name__ == '__main__':
    main()
