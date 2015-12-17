//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// FIXME: Workaround for inability to create existentials of protocols
// with associated types <rdar://problem/11689181>

// This file contains "existentials" for the protocols defined in
// Policy.swift.  Similar components should usually be defined next to
// their respective protocols.

internal struct _CollectionOf<
  IndexType_ : ForwardIndex, T
> : Collection {
  init(startIndex: IndexType_, endIndex: IndexType_,
      _ subscriptImpl: (IndexType_)->T) {
    self.startIndex = startIndex
    self.endIndex = endIndex
    _subscriptImpl = subscriptImpl
  }

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  func iterator() -> AnyIterator<T> {
    var index = startIndex
    return AnyIterator {
      () -> T? in
      if _fastPath(index != self.endIndex) {
        index._successorInPlace()
        return self._subscriptImpl(index)
      }
      return nil
    }
  }

  let startIndex: IndexType_
  let endIndex: IndexType_

  subscript(i: IndexType_) -> T {
    return _subscriptImpl(i)
  }

  let _subscriptImpl: (IndexType_)->T
}

