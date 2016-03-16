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

def collectionForTraversal(traversal):
    if traversal == 'Forward':
        return 'Collection'
    if traversal == 'Bidirectional':
        return 'BidirectionalCollection'
    if traversal == 'RandomAccess':
        return 'RandomAccessCollection'
    assert False, 'unknown traversal'

