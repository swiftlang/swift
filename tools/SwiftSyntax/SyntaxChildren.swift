//===------------- SyntaxChildren.swift - Syntax Child Iterator -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

public struct SyntaxChildren: Sequence {
  public struct Iterator: IteratorProtocol {
    let node: Syntax
    var indexIterator: _SyntaxBase.PresentChildIndicesSequence.Iterator

    init(node: Syntax) {
      self.indexIterator = node.presentChildIndices.makeIterator()
      self.node = node
    }

    public mutating func next() -> Syntax? {
      guard let index = indexIterator.next() else { return nil }
      return node.child(at: index)
    }
  }

  let node: Syntax

  public func makeIterator() -> SyntaxChildren.Iterator {
    return Iterator(node: node)
  }
}
