#!/usr/bin/env python

# This script outputs a Swift source with randomly-generated type definitions,
# which can be used for ABI or layout algorithm fuzzing.

# TODO: generate types with generics, existentials, compositions
# TODO: prevent cycles in inline storage

import collections
import random

maxDepth = 5
maxMembers = 5
typesToDefine = collections.deque([0])
nextName = 1
random.seed()


def randomTypeList(depth):
    count = random.randint(0, maxMembers)
    result = "("
    for i in xrange(count):
        if i > 0:
            result += ", "
        result += randomTypeReference(depth + 1)
    result += ")"
    return result


def randomTypeReference(depth):
    def nominal():
        global nextName
        global typesToDefine
        which = random.randint(0,
                               nextName if depth < maxDepth else nextName - 1)
        if which == nextName:
            typesToDefine.append(which)
            nextName += 1
        return "T" + str(which)

    def tuple():
        return randomTypeList(depth + 1)

    def metatype():
        return "(" + randomTypeReference(depth + 1) + ").Type"

    def leaf():
        leaves = ["Int", "String", "Int8", "Int16", "Int32", "Int64"]
        return random.choice(leaves)

    if depth < maxDepth:
        kinds = [nominal, tuple, metatype, leaf, leaf, leaf, leaf, leaf]
    else:
        kinds = [leaf]
    return random.choice(kinds)()


def defineRandomProduct(kind, name, depth):
    print(kind + " " + name + " {")
    # Suppress errors about missing initializers
    print("  init() { fatalError() }")

    numMembers = random.randint(0, maxMembers)
    for i in xrange(numMembers):
        print("  var x" + str(i) + ": " + randomTypeReference(depth + 1))

    print("}")


def defineRandomEnum(name, depth):
    # TODO: indirect cases
    print("enum " + name + " {")

    numCases = random.randint(0, maxMembers)
    for i in xrange(numCases):
        print("  case x" + str(i) + randomTypeList(depth + 1))

    print("}")


def defineRandomType(name, depth):
    def struct():
        defineRandomProduct("struct", name, depth)

    def clas():
        defineRandomProduct("class", name, depth)

    def enum():
        defineRandomEnum(name, depth)

    kinds = [struct, clas, enum]
    return random.choice(kinds)()


while len(typesToDefine) > 0:
    ty = typesToDefine.popleft()
    defineRandomType("T" + str(ty), 0)
