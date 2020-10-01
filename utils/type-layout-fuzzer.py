#!/usr/bin/env python

# This script outputs a Swift source with randomly-generated type definitions,
# which can be used for ABI or layout algorithm fuzzing.

# TODO: generate types with generics, existentials, compositions

from __future__ import print_function

import random
import sys

maxDepth = 5
maxMembers = 5
typesDefined = []
classesDefined = []
nextToDefine = 0
objcInterop = False

if len(sys.argv) >= 2:
    if sys.argv[1] == "--objc":
        objcInterop = True
    if sys.argv[1] == "--help":
        print("Usage: " + sys.argv[0] + " [--objc]", file=sys.stderr)
        print("", file=sys.stderr)
        print("  --objc          Include ObjC-interop types", file=sys.stderr)
        sys.exit(2)

random.seed()
if objcInterop:
    print("import Foundation")
    print()


def randomTypeList(depth):
    count = random.randint(0, maxMembers)
    result = "("
    for i in range(count):
        if i > 0:
            result += ", "
        result += randomTypeReference(depth + 1)
    result += ")"
    return result


def randomTypeReference(depth):
    def nominal():
        global typesDefined
        allowNew = depth < maxDepth
        bound = len(classesDefined) if allowNew else len(classesDefined) - 1
        which = random.randint(0, bound)
        if which < len(classesDefined):
            return classesDefined[which]
        newName = "T" + str(len(typesDefined))

        def defineRandomRelatedType(name):
            defineRandomNominalType(name, depth)

        typesDefined.append((newName, defineRandomRelatedType))
        return newName

    def tuple():
        return randomTypeList(depth + 1)

    def metatype():
        return "(" + randomTypeReference(depth + 1) + ").Type"

    def leaf():
        leaves = ["Int", "String", "Int8", "Int16", "Int32", "Int64",
                  "(() -> ())", "(@convention(c) () -> ())", "AnyObject"]
        if objcInterop:
            leaves += ["NSObject", "(@convention(block) () -> ())"]
        return random.choice(leaves)

    if depth < maxDepth:
        kinds = [nominal, tuple, metatype, leaf, leaf, leaf, leaf, leaf]
    else:
        kinds = [leaf]
    return random.choice(kinds)()


def defineRandomFields(depth, basename):
    numMembers = random.randint(0, maxMembers)
    for i in range(numMembers):
        print("  var " + basename + str(i) + ": " +
              randomTypeReference(depth + 1))


def defineRandomClass(name, depth):
    global classesDefined
    classesDefined.append(name)
    print("class " + name, end="")

    def inheritNSObject():
        print(": NSObject", end="")

    def inheritsOtherClass():
        print(": ", end="")
        name = "T" + str(len(typesDefined))

        def defineRandomBaseClass(name):
            defineRandomClass(name, depth)

        typesDefined.append((name, defineRandomBaseClass))
        print(name, end="")

    def inheritsNothing():
        pass

    inheritances = [inheritsNothing]
    if depth == 0:
        # The contents of classes are interesting only for top-level type
        inheritances += [inheritsOtherClass]

    if objcInterop:
        inheritances += [inheritNSObject]
    random.choice(inheritances)()

    print(" {")
    # Prevent errors about lack of initializers
    print("  init(" + name + ": ()) { fatalError() }")
    # The contents of classes are interesting only for top-level type
    if depth == 0:
        defineRandomFields(depth, "x" + name)
    print("}")
    print()


def defineRandomNominalType(name, depth=0):
    def struct():
        print("struct " + name + " {")
        defineRandomFields(depth, "x")
        print("}")
        print()

    def clas():
        defineRandomClass(name, depth)

    def enum():
        # TODO: indirect cases
        print("enum " + name + " {")

        numCases = random.randint(0, maxMembers)
        for i in range(numCases):
            print("  case x" + str(i) + randomTypeList(depth + 1))

        print("}")
        print()

    kinds = [struct, clas, enum]
    return random.choice(kinds)()


typesDefined.append(("Generated", defineRandomNominalType))

while nextToDefine < len(typesDefined):
    name, definer = typesDefined[nextToDefine]
    definer(name)
    nextToDefine += 1
