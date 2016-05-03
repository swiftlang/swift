# ===--- gyb_stdlib_support.py -----------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

TRAVERSALS = ['Forward', 'Bidirectional', 'RandomAccess']


def collectionForTraversal(traversal):  # noqa (N802 function name should be lowercase)
    if traversal == 'Forward':
        return 'Collection'
    if traversal == 'Bidirectional':
        return 'BidirectionalCollection'
    if traversal == 'RandomAccess':
        return 'RandomAccessCollection'
    assert False, 'unknown traversal'


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
    if traversal == 'Bidirectional':
        return 'DefaultBidirectionalIndices'
    if traversal == 'RandomAccess':
        return 'DefaultRandomAccessIndices'
    assert False, 'unknown traversal'
