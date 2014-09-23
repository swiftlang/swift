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

//////////////////////////////////////////
// FIXME: Workaround for inability to create existentials of protocols
// with associated types <rdar://problem/11689181>

// This file contains "existentials" for the protocols defined in
// Policy.swift.  Similar components should usually be defined next to
// their respective protocols.
public struct GeneratorOf<T> : GeneratorType, SequenceType {
  public init(_ next: ()->T?) {
    self._next = next
  }
  
  public init<G: GeneratorType where G.Element == T>(var _ self_: G) {
    self._next = { self_.next() }
  }
  
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: `next()` has not been applied to a copy of `self`
  /// since the copy was made, and no preceding call to `self.next()`
  /// has returned `nil`.
  public mutating func next() -> T? {
    return _next()
  }

  public func generate() -> GeneratorOf {
    return self
  }
  let _next: ()->T?
}

public struct SequenceOf<T> : SequenceType {
  public init<G: GeneratorType where G.Element == T>(_ generate: ()->G) {
    _generate = { GeneratorOf(generate()) }
  }
  public init<S: SequenceType where S.Generator.Element == T>(_ self_: S) {
    self = SequenceOf({ self_.generate() })
  }

  public func generate() -> GeneratorOf<T> {
    return _generate()
  }
  
  let _generate: ()->GeneratorOf<T>
}

internal struct _CollectionOf<
  IndexType_ : ForwardIndexType, T
> : CollectionType {
  init(startIndex: IndexType_, endIndex: IndexType_,
      _ subscriptImpl: (IndexType_)->T) {
    self.startIndex = startIndex
    self.endIndex = endIndex
    _subscriptImpl = subscriptImpl
  }

  func generate() -> GeneratorOf<T> {
    var index = startIndex
    return GeneratorOf {
      () -> T? in
      if _fastPath(index != self.endIndex) {
        ++index
        return self._subscriptImpl(index)
      }
      return .None
    }
  }

  let startIndex: IndexType_
  let endIndex: IndexType_

  subscript(i: IndexType_) -> T {
    return _subscriptImpl(i)
  }

  let _subscriptImpl: (IndexType_)->T
}

public struct SinkOf<T> : SinkType {
  public init(_ put: (T)->()) {
    _put = put
  }

  public init<S: SinkType where S.Element == T>(var _ base: S) {
    _put = { base.put($0) }
  }
  public func put(x: T) {
    _put(x)
  }
  let _put: (T)->()
}

