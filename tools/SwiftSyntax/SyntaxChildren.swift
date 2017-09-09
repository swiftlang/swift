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
    var index: Int = 0
    let node: Syntax

    public mutating func next() -> Syntax? {
      defer { index += 1 }
      return node.child(at: index)
    }
  }
  let node: Syntax

  public func makeIterator() -> SyntaxChildren.Iterator {
    return Iterator(index: 0, node: node)
  }
}
