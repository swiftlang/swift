# cmpcodesize/compare.py - Compare sizes of built products -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import print_function

import re
import os
import subprocess
import collections
from operator import itemgetter

Prefixes = {
    # Cpp
    "__Z": "CPP",
    "_swift": "CPP",
    "__swift": "CPP",

    # Objective-C
    "+[": "ObjC",
    "-[": "ObjC",

    # Swift
    "__TP": "Partial Apply",
    "__TTW": "Protocol Witness",
    "__Tw": "Value Witness",
    "__TM": "Type Metadata",
    "__TF": "Swift Function",
    "__TTSg": "Generic Spec",
    "__TTSf": "FuncSig Spec",
    "__TZF": "Static Func",
    # Function signature specialization of a generic specialization.
    "__TTSGF": "FuncSigGen Spec",
    "__TTo": "Swift @objc Func",
}

Infixes = {
    #Swift
    "q_": "Generic Function"
}

GenericFunctionPrefix = "__TTSg"

SortedPrefixes = sorted(Prefixes)
SortedInfixes = sorted(Infixes)


def addFunction(sizes, function, start_addr, end_addr, group_by_prefix):
    if not function or start_addr is None or end_addr is None:
        return

    size = end_addr - start_addr

    if group_by_prefix:
        for infix in SortedInfixes:
	    if infix in function:
               if GenericFunctionPrefix not in function:
	           sizes[Infixes[infix]] += size
                   return
        for prefix in SortedPrefixes:
	    if function.startswith(prefix):
                # Special handling for function signature specializations
                # of generic specializations.
                if prefix == "__TTSf" and GenericFunctionPrefix in function:
                    prefix = "__TTSGF"
                sizes[Prefixes[prefix]] += size
                return
        sizes["Unknown"] += size
    else:
        sizes[function] += size


def flatten(*args):
    for x in args:
        if hasattr(x, '__iter__'):
            for y in flatten(*x):
                yield y
        else:
            yield x


def readSizes(sizes, file_name, function_details, group_by_prefix):
    # Check if multiple architectures are supported by the object file.
    # Prefer arm64 if available.
    architectures = subprocess.check_output(["otool", "-V", "-f", file_name]).split("\n")
    arch = None
    archPattern = re.compile('architecture ([\S]+)')
    for architecture in architectures:
        archMatch = archPattern.match(architecture)
        if archMatch:
            if arch is None:
                arch = archMatch.group(1)
            if "arm64" in arch:
                arch = "arm64"
    if arch is not None:
      archParams = ["-arch", arch]
    else:
      archParams = []

    if function_details:
        content = subprocess.check_output(flatten(["otool", archParams, "-l", "-v", "-t", file_name])).split("\n")
        content += subprocess.check_output(flatten(["otool", archParams, "-v", "-s", "__TEXT", "__textcoal_nt", file_name])).split("\n")
    else:
        content = subprocess.check_output(flatten(["otool", archParams, "-l", file_name])).split("\n")

    sectName = None
    currFunc = None
    start_addr = None
    end_addr = None

    sectionPattern = re.compile(' +sectname ([\S]+)')
    sizePattern = re.compile(' +size ([\da-fx]+)')
    asmlinePattern = re.compile('^([0-9a-fA-F]+)\s')
    labelPattern = re.compile('^((\-*\[[^\]]*\])|[^\/\s]+):$')

    for line in content:
        asmlineMatch = asmlinePattern.match(line)
        if asmlineMatch:
            addr = int(asmlineMatch.group(1), 16)
            if start_addr is None:
                start_addr = addr
            end_addr = addr
        elif line == "Section":
            sectName = None
        else:
            labelMatch = labelPattern.match(line)
            sizeMatch = sizePattern.match(line)
            sectionMatch = sectionPattern.match(line)
            if labelMatch:
                funcName = labelMatch.group(1)
                addFunction(sizes, currFunc, start_addr, end_addr, group_by_prefix)
                currFunc = funcName
                start_addr = None
                end_addr = None
            elif sizeMatch and sectName and group_by_prefix:
                size = int(sizeMatch.group(1), 16)
                sizes[sectName] += size
            elif sectionMatch:
                sectName = sectionMatch.group(1)
                if sectName == "__textcoal_nt":
                    sectName = "__text"

    addFunction(sizes, currFunc, start_addr, end_addr, group_by_prefix)


def compareSizes(old_sizes, new_sizes, name_key, title):
    oldSize = old_sizes[name_key]
    newSize = new_sizes[name_key]
    if oldSize is not None and newSize is not None:
        if oldSize != 0:
            perc = "%.1f%%" % ((1.0 - float(newSize) / float(oldSize)) * 100.0)
        else:
            perc = "- "
        print("%-26s%16s: %8d  %8d  %6s" % (title, name_key, oldSize, newSize, perc))


def compareSizesOfFile(old_files, new_files, all_sections, list_categories):
    old_sizes = collections.defaultdict(int)
    new_sizes = collections.defaultdict(int)
    for oldFile in old_files:
        readSizes(old_sizes, oldFile, list_categories, True)
    for newFile in new_files:
        readSizes(new_sizes, newFile, list_categories, True)

    if len(old_files) == 1 and len(new_files) == 1:
        oldBase = os.path.basename(old_files[0])
        newBase = os.path.basename(new_files[0])
        title = oldBase
        if oldBase != newBase:
            title += "-" + newBase
    else:
        title = "old-new"

    compareSizes(old_sizes, new_sizes, "__text", title)
    if list_categories:
        prev = None
        for categoryName in sorted(Prefixes.values()) + sorted(Infixes.values()) + ["Unknown"]:
            if categoryName != prev:
                compareSizes(old_sizes, new_sizes, categoryName, "")
            prev = categoryName

    if all_sections:
        sectionTitle = "    section"
        compareSizes(old_sizes, new_sizes, "__textcoal_nt", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__stubs", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__const", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__cstring", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__objc_methname", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__const", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__objc_const", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__data", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__swift1_proto", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__common", sectionTitle)
        compareSizes(old_sizes, new_sizes, "__bss", sectionTitle)


def listFunctionSizes(size_array):
    for pair in sorted(size_array, key=itemgetter(1)):
        name = pair[0]
        size = pair[1]
        yield "%8d %s" % (size, name)


def compareFunctionSizes(old_files, new_files):
    old_sizes = collections.defaultdict(int)
    new_sizes = collections.defaultdict(int)
    for name in old_files:
        readSizes(old_sizes, name, True, False)
    for name in new_files:
        readSizes(new_sizes, name, True, False)

    onlyInFile1 = []
    onlyInFile2 = []
    inBoth = []

    onlyInFile1Size = 0
    onlyInFile2Size = 0
    inBothSize = 0

    for func, oldSize in old_sizes.items():
        newSize = new_sizes[func]
        if newSize != 0:
            inBoth.append((func, oldSize, newSize))
        else:
            onlyInFile1.append((func, oldSize))
            onlyInFile1Size += oldSize

    for func, newSize in new_sizes.items():
        oldSize = old_sizes[func]
        if oldSize == 0:
            onlyInFile2.append((func, newSize))
            onlyInFile2Size += newSize

    if onlyInFile1:
        print("Only in old file(s)")
        print(listFunctionSizes(onlyInFile1))
        print("Total size of functions only in old file: {}".format(onlyInFile1Size))
        print()

    if onlyInFile2:
        print("Only in new files(s)")
        print(listFunctionSizes(onlyInFile2))
        print("Total size of functions only in new file: {}".format(onlyInFile2Size))
        print()

    if inBoth:
        sizeIncrease = 0
        sizeDecrease = 0
        print("%8s %8s %8s" % ("old", "new", "diff"))
        for triple in sorted(inBoth, key=lambda tup: (tup[2] - tup[1], tup[1])):
            func = triple[0]
            oldSize = triple[1]
            newSize = triple[2]
            diff = newSize - oldSize
            if diff > 0:
                sizeIncrease += diff
            else:
                sizeDecrease -= diff
            if diff == 0:
                inBothSize += newSize
            print("%8d %8d %8d %s" % (oldSize, newSize, newSize - oldSize, func))
        print("Total size of functions with the same size in both files: {}".format(inBothSize))
        print("Total size of functions that got smaller: {}".format(sizeDecrease))
        print("Total size of functions that got bigger: {}".format(sizeIncrease))
        print("Total size change of functions present in both files: {}".format(sizeIncrease - sizeDecrease))
