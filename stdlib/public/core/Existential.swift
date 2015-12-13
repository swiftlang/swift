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

/// Unavailable; use `AnyGenerator<T>` instead.
@available(*, unavailable, renamed="AnyGenerator")
public struct GeneratorOf<T> {}

/// Unavailable; use `AnySequence<T>` instead.
@available(*, unavailable, renamed="AnySequence")
public struct SequenceOf<T> {}

internal struct _CollectionOf<
  IndexType_ : ForwardIndexType, T
> : CollectionType {
  init(startIndex: IndexType_, endIndex: IndexType_,
      _ subscriptImpl: (IndexType_)->T) {
    self.startIndex = startIndex
    self.endIndex = endIndex
    _subscriptImpl = subscriptImpl
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  func generate() -> AnyGenerator<T> {
    var index = startIndex
    return AnyGenerator {
      () -> T? in
      if _fastPath(index != self.endIndex) {
        ++index
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

@available(*, unavailable, message="SinkOf has been removed. Use (T)->() closures directly instead.")
public struct SinkOf<T> {}

