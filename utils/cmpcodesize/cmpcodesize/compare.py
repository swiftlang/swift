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
    "__Z" : "CPP",
    "_swift" : "CPP",
    "__swift" : "CPP",

    # Objective-C
    "+[" : "ObjC",
    "-[" : "ObjC",

    # Swift
    "__TP"  : "Partial Apply",
    "__TTW" : "Protocol Witness",
    "__Tw"  : "Value Witness",
    "__TM"  : "Type Metadata",
    "__TF"  : "Swift Function",
    "__TTSg" : "Generic Spec",
    "__TTSf" : "FuncSig Spec",
    "__TZF" : "Static Func",
    # Function signature specialization of a generic specialization.
    "__TTSGF" : "FuncSigGen Spec",
    "__TTo" : "Swift @objc Func",
}

Infixes = {
  #Swift
  "q_" : "Generic Function"
}

GenericFunctionPrefix = "__TTSg"

SortedPrefixes = sorted(Prefixes)
SortedInfixes = sorted(Infixes)


def addFunction(sizes, function, startAddr, endAddr, groupByPrefix):
    if not function or startAddr is None or endAddr is None:
        return

    size = endAddr - startAddr

    if groupByPrefix:
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


def readSizes(sizes, fileName, functionDetails, groupByPrefix):
    # Check if multiple architectures are supported by the object file.
    # Prefer arm64 if available.
    architectures = subprocess.check_output(["otool", "-V", "-f", fileName]).split("\n")
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

    if functionDetails:
        content = subprocess.check_output(flatten(["otool", archParams, "-l", "-v", "-t", fileName])).split("\n")
        content += subprocess.check_output(flatten(["otool", archParams, "-v", "-s", "__TEXT", "__textcoal_nt", fileName])).split("\n")
    else:
        content = subprocess.check_output(flatten(["otool", archParams, "-l", fileName])).split("\n")

    sectName = None
    currFunc = None
    startAddr = None
    endAddr = None

    sectionPattern = re.compile(' +sectname ([\S]+)')
    sizePattern = re.compile(' +size ([\da-fx]+)')
    asmlinePattern = re.compile('^([0-9a-fA-F]+)\s')
    labelPattern = re.compile('^((\-*\[[^\]]*\])|[^\/\s]+):$')

    for line in content:
        asmlineMatch = asmlinePattern.match(line)
        if asmlineMatch:
            addr = int(asmlineMatch.group(1), 16)
            if startAddr is None:
                startAddr = addr
            endAddr = addr
        elif line == "Section":
            sectName = None
        else:
            labelMatch = labelPattern.match(line)
            sizeMatch = sizePattern.match(line)
            sectionMatch = sectionPattern.match(line)
            if labelMatch:
                funcName = labelMatch.group(1)
                addFunction(sizes, currFunc, startAddr, endAddr, groupByPrefix)
                currFunc = funcName
                startAddr = None
                endAddr = None
            elif sizeMatch and sectName and groupByPrefix:
                size = int(sizeMatch.group(1), 16)
                sizes[sectName] += size
            elif sectionMatch:
                sectName = sectionMatch.group(1)
                if sectName == "__textcoal_nt":
                    sectName = "__text"

    addFunction(sizes, currFunc, startAddr, endAddr, groupByPrefix)


def compareSizes(oldSizes, newSizes, nameKey, title):
    oldSize = oldSizes[nameKey]
    newSize = newSizes[nameKey]
    if oldSize is not None and newSize is not None:
        if oldSize != 0:
            perc = "%.1f%%" % ((1.0 - float(newSize) / float(oldSize)) * 100.0)
        else:
            perc = "- "
        print("%-26s%16s: %8d  %8d  %6s" % (title, nameKey, oldSize, newSize, perc))


def compareSizesOfFile(oldFiles, newFiles, allSections, listCategories):
    oldSizes = collections.defaultdict(int)
    newSizes = collections.defaultdict(int)
    for oldFile in oldFiles:
        readSizes(oldSizes, oldFile, listCategories, True)
    for newFile in newFiles:
        readSizes(newSizes, newFile, listCategories, True)

    if len(oldFiles) == 1 and len(newFiles) == 1:
        oldBase = os.path.basename(oldFiles[0])
        newBase = os.path.basename(newFiles[0])
        title = oldBase
        if oldBase != newBase:
            title += "-" + newBase
    else:
        title = "old-new"

    compareSizes(oldSizes, newSizes, "__text", title)
    if listCategories:
        prev = None
        for categoryName in sorted(Prefixes.values()) + sorted(Infixes.values())+ ["Unknown"]:
            if categoryName != prev:
                compareSizes(oldSizes, newSizes, categoryName, "")
            prev = categoryName

    if allSections:
        sectionTitle = "    section"
        compareSizes(oldSizes, newSizes, "__textcoal_nt", sectionTitle)
        compareSizes(oldSizes, newSizes, "__stubs", sectionTitle)
        compareSizes(oldSizes, newSizes, "__const", sectionTitle)
        compareSizes(oldSizes, newSizes, "__cstring", sectionTitle)
        compareSizes(oldSizes, newSizes, "__objc_methname", sectionTitle)
        compareSizes(oldSizes, newSizes, "__const", sectionTitle)
        compareSizes(oldSizes, newSizes, "__objc_const", sectionTitle)
        compareSizes(oldSizes, newSizes, "__data", sectionTitle)
        compareSizes(oldSizes, newSizes, "__swift1_proto", sectionTitle)
        compareSizes(oldSizes, newSizes, "__common", sectionTitle)
        compareSizes(oldSizes, newSizes, "__bss", sectionTitle)


def listFunctionSizes(sizeArray):
    for pair in sorted(sizeArray, key=itemgetter(1)):
        name = pair[0]
        size = pair[1]
        return "%8d %s" % (size, name)


def compareFunctionSizes(oldFiles, newFiles):
    oldSizes = collections.defaultdict(int)
    newSizes = collections.defaultdict(int)
    for name in oldFiles:
        readSizes(oldSizes, name, True, False)
    for name in newFiles:
        readSizes(newSizes, name, True, False)

    onlyInFile1 = []
    onlyInFile2 = []
    inBoth = []

    onlyInFile1Size = 0
    onlyInFile2Size = 0
    inBothSize = 0

    for func, oldSize in oldSizes.items():
        newSize = newSizes[func]
        if newSize != 0:
            inBoth.append((func, oldSize, newSize))
        else:
            onlyInFile1.append((func, oldSize))
            onlyInFile1Size += oldSize

    for func, newSize in newSizes.items():
        oldSize = oldSizes[func]
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
            print("%8d %8d %8d %s" %(oldSize, newSize, newSize - oldSize, func))
        print("Total size of functions with the same size in both files: {}".format(inBothSize))
        print("Total size of functions that got smaller: {}".format(sizeDecrease))
        print("Total size of functions that got bigger: {}".format(sizeIncrease))
        print("Total size change of functions present in both files: {}".format(sizeIncrease - sizeDecrease))
