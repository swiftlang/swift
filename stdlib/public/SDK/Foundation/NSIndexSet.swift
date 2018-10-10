//===----------------------------------------------------------------------===//
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

@_exported import Foundation // Clang module

// TODO: Evaluate deprecating with a message in favor of IndexSet.
public struct NSIndexSetIterator : IteratorProtocol {
  public typealias Element = Int

  internal let _set: NSIndexSet
  internal var _first: Bool = true
  internal var _current: Int?

  internal init(set: NSIndexSet) {
    self._set = set
    self._current = nil
  }

  public mutating func next() -> Int? {
    if _first {
      _current = _set.firstIndex
      _first = false
    } else if let c = _current {
      _current = _set.indexGreaterThanIndex(c)
    } else {
      // current is already nil
    }
    if _current == NSNotFound {
      _current = nil
    }
    return _current
  }
}

extension NSIndexSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSIndexSetIterator {
    return NSIndexSetIterator(set: self)
  }
}
