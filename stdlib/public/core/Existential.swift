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

/// Deprecated; use `AnyGenerator<T>` instead
@availability(*, deprecated, renamed="AnyGenerator")
public struct GeneratorOf<T> : GeneratorType, SequenceType {

  /// Deprecated; use `anyGenerator { ... }`` instead
  @availability(*, deprecated, renamed="anyGenerator")
  public init(_ nextElement: ()->T?) {
    self._next = nextElement
  }
  
  /// Deprecated; use `anyGenerator(base)`` instead
  @availability(*, deprecated, renamed="anyGenerator")
  public init<G: GeneratorType where G.Element == T>(var _ base: G) {
    self._next = { base.next() }
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

  /// `GeneratorOf<T>` is also a `SequenceType`, so it `generate`\ s
  /// a copy of itself
  public func generate() -> GeneratorOf {
    return self
  }
  let _next: ()->T?
}

/// Deprecated; use `AnySequence<T>` instead
@availability(*, deprecated, renamed="AnySequence")
public struct SequenceOf<T> : SequenceType {
  /// Construct an instance whose `generate()` method forwards to
  /// `makeUnderlyingGenerator`
  public init<G: GeneratorType where G.Element == T>(
    _ makeUnderlyingGenerator: ()->G
  ) {
    _generate = { GeneratorOf(makeUnderlyingGenerator()) }
  }
  
  /// Construct an instance whose `generate()` method forwards to
  /// that of `base`.
  public init<S: SequenceType where S.Generator.Element == T>(_ base: S) {
    self = SequenceOf({ base.generate() })
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
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

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  func generate() -> AnyGenerator<T> {
    var index = startIndex
    return anyGenerator {
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

/// A type-erased sink.
///
/// Forwards operations to an arbitrary underlying sink with the same
/// `Element` type, hiding the specifics of the underlying sink type.
public struct SinkOf<T> : SinkType {
  /// Construct an instance whose `put(x)` calls `putElement(x)`
  public init(_ putElement: (T)->()) {
    _put = putElement
  }

  /// Construct an instance whose `put(x)` calls `base.put(x)`
  public init<S: SinkType where S.Element == T>(var _ base: S) {
    _put = { base.put($0) }
  }
  
  /// Write `x` to this sink.
  public func put(x: T) {
    _put(x)
  }
  
  let _put: (T)->()
}

