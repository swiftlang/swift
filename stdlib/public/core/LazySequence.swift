//===--- LazySequence.swift -----------------------------------------------===//
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

public protocol _prext_LazySequenceType : SequenceType {
  /// A SequenceType that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// This associated type is used to keep the result type of
  /// `lazy(x).operation` from growing a `_prext_LazySequence` layer.
  typealias Elements: SequenceType = Self

  /// A sequence containing the same elements as this one, possibly with
  /// a simpler type.
  ///
  /// When implementing lazy operations, wrapping `elements` instead
  /// of `self` can prevent result types from growing a `_prext_LazySequence`
  /// layer.
  ///
  /// Note: this property need not be implemented by conforming types,
  /// it has a default implementation in a protocol extension that
  /// just returns `self`.
  var elements: Elements {get} 
  
  /// An Array, created on-demand, containing the elements of this
  /// lazy SequenceType.
  ///
  /// Note: this property need not be implemented by conforming types, it has a
  /// default implementation in a protocol extension.
  var array: [Generator.Element] {get}
}

extension _prext_LazySequenceType {
  /// an Array, created on-demand, containing the elements of this
  /// lazy SequenceType.
  public var array: [Generator.Element] {
    return Array(self)
  }
}

extension _prext_LazySequenceType where Elements == Self {
  public var elements: Self { return self }
}

/// A sequence that forwards its implementation to an underlying
/// sequence instance while exposing lazy computations as methods.
public struct _prext_LazySequence<Base_ : SequenceType>
  : _prext_LazySequenceType, _SequenceWrapperType {
  public var _base: Base_
  public var elements: Base_ { return _base }
}

extension SequenceType {
  public var _prext_lazy: _prext_LazySequence<Self> {
    return _prext_LazySequence(_base: self)
  }
}

extension _prext_LazySequenceType {
  public var _prext_lazy: Self { // Don't re-wrap already-lazy sequences
    return self
  }
}

//===----------------------------------------------------------------------===//
// Implementations we are replacing
//===----------------------------------------------------------------------===//

/// A sequence that forwards its implementation to an underlying
/// sequence instance while exposing lazy computations as methods.
public struct LazySequence<Base : SequenceType> : SequenceType {
  @available(*, unavailable, renamed="Base")
  public typealias S = Base

  /// Construct an instance with `base` as its underlying sequence
  /// instance.
  public init(_ base: Base) {
    self._base = base
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> Base.Generator {
    return self._base.generate()
  }

  /// an Array, created on-demand, containing the elements of this
  /// lazy SequenceType.
  public var array: [Base.Generator.Element] {
    return Array(_base)
  }

  public func underestimateCount() -> Int {
    return _base.underestimateCount()
  }

  var _base: Base
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
public func lazy<Base : SequenceType>(s: Base) -> LazySequence<Base> {
  return LazySequence(s)
}
