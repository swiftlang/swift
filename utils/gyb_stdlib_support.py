# ===--- gyb_stdlib_support.py -----------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

TRAVERSALS = ['Forward', 'Bidirectional', 'RandomAccess']


def collectionForTraversal(traversal):  # noqa (N802 function name should be lowercase)
    if traversal == 'Forward':
        return 'Collection'
    elif traversal == 'Bidirectional':
        return 'BidirectionalCollection'
    elif traversal == 'RandomAccess':
        return 'RandomAccessCollection'
    else:
        raise ValueError("Unknown traversal %r" % traversal)


def sliceTypeName(traversal, mutable, rangeReplaceable):  # noqa (N802)
    name = collectionForTraversal(traversal).replace('Collection', 'Slice')
    if rangeReplaceable:
        name = 'RangeReplaceable' + name
    if mutable:
        name = 'Mutable' + name
    return name


def protocolsForCollectionFeatures(traversal, mutable, rangeReplaceable):  # noqa (N802)
    protocols = [collectionForTraversal(traversal)]
    if mutable:
        protocols.append('MutableCollection')
    if rangeReplaceable:
        protocols.append('RangeReplaceableCollection')
    return protocols


def defaultIndicesForTraversal(traversal):  # noqa (N802)
    if traversal == 'Forward':
        return 'DefaultIndices'
    elif traversal == 'Bidirectional':
        return 'DefaultBidirectionalIndices'
    elif traversal == 'RandomAccess':
        return 'DefaultRandomAccessIndices'
    else:
        raise ValueError("Unknown traversal %r" % traversal)


def documentationNameForTraversal(traversal):  # noqa (N802)
    if traversal == 'Forward':
        return 'collection'
    elif traversal == 'Bidirectional':
        return 'bidirectional collection'
    elif traversal == 'RandomAccess':
        return 'random-access collection'
    else:
        raise ValueError("Unknown traversal %r" % traversal)
