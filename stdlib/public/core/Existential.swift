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

// This file contains "existentials" for the protocols defined in
// Policy.swift.  Similar components should usually be defined next to
// their respective protocols.

@_fixed_layout // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal struct _CollectionOf<
  IndexType : Strideable, Element
> : Collection {

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal init(
    _startIndex: IndexType, endIndex: IndexType,
    _ subscriptImpl: @escaping (IndexType) -> Element
  ) {
    self.startIndex = _startIndex
    self.endIndex = endIndex
    self._subscriptImpl = subscriptImpl
  }

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func makeIterator() -> AnyIterator<Element> {
    var index = startIndex
    return AnyIterator {
      () -> Element? in
      if _fastPath(index != self.endIndex) {
        self.formIndex(after: &index)
        return self._subscriptImpl(index)
      }
      return nil
    }
  }

  @_versioned // FIXME(sil-serialize-all)
  internal let startIndex: IndexType
  @_versioned // FIXME(sil-serialize-all)
  internal let endIndex: IndexType

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func index(after i: IndexType) -> IndexType {
    return i.advanced(by: 1)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal subscript(i: IndexType) -> Element {
    return _subscriptImpl(i)
  }

  @_versioned // FIXME(sil-serialize-all)
  internal let _subscriptImpl: (IndexType) -> Element
}

