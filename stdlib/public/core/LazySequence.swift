//===--- LazySequence.swift -----------------------------------------------===//
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

/// A sequence on which normally-eager sequence operations are implemented
/// lazily.
///
/// Lazy sequences can be used to avoid needless storage allocation
/// and computation, because they use an underlying sequence for
/// storage and compute their elements on demand. For example, `doubled` in
/// this code sample is a sequence containing the values `2`, `4`, and `6`.
///
///     let doubled = [1, 2, 3].lazy.map { $0 * 2 }
///
/// Each time an element of the lazy sequence `doubled` is accessed, the 
/// closure accesses and transforms an element of the underlying array.
///
/// Sequence operations that take closure arguments, such as `map(_:)` and
/// `filter(_:)`, are normally eager: They use the closure immediately and
/// return a new array. When you use the `lazy` property, you give the standard
/// library explicit permission to store the closure and the sequence
/// in the result, and defer computation until it is needed.
///
/// ## Adding New Lazy Operations
///
/// To add a new lazy sequence operation, extend this protocol with
/// a method that returns a lazy wrapper that itself conforms to
/// `LazySequenceProtocol`.  For example, an eager `scan(_:_:)`
/// method is defined as follows:
///
///     extension Sequence {
///         /// Returns an array containing the results of
///         ///
///         ///   p.reduce(initial, nextPartialResult)
///         ///
///         /// for each prefix `p` of `self`, in order from shortest to
///         /// longest. For example:
///         ///
///         ///     (1..<6).scan(0, +) // [0, 1, 3, 6, 10, 15]
///         ///
///         /// - Complexity: O(n)
///         func scan<Result>(
///             _ initial: Result,
///             _ nextPartialResult: (Result, Element) -> Result
///         ) -> [Result] {
///             var result = [initial]
///             for x in self {
///                 result.append(nextPartialResult(result.last!, x))
///             }
///             return result
///         }
///     }
///
/// You can build a sequence type that lazily computes the elements in the
/// result of a scan:
///
///     struct LazyScanSequence<Base: Sequence, Result>
///         : LazySequenceProtocol
///     {
///         let initial: Result
///         let base: Base
///         let nextPartialResult:
///             (Result, Base.Element) -> Result
///
///         struct Iterator: IteratorProtocol {
///             var base: Base.Iterator
///             var nextElement: Result?
///             let nextPartialResult:
///                 (Result, Base.Element) -> Result
///             
///             mutating func next() -> Result? {
///                 return nextElement.map { result in
///                     nextElement = base.next().map {
///                         nextPartialResult(result, $0)
///                     }
///                     return result
///                 }
///             }
///         }
///         
///         func makeIterator() -> Iterator {
///             return Iterator(
///                 base: base.makeIterator(),
///                 nextElement: initial as Result?,
///                 nextPartialResult: nextPartialResult)
///         }
///     }
///
/// Finally, you can give all lazy sequences a lazy `scan(_:_:)` method:
///     
///     extension LazySequenceProtocol {
///         func scan<Result>(
///             _ initial: Result,
///             _ nextPartialResult: @escaping (Result, Element) -> Result
///         ) -> LazyScanSequence<Self, Result> {
///             return LazyScanSequence(
///                 initial: initial, base: self, nextPartialResult: nextPartialResult)
///         }
///     }
///
/// With this type and extension method, you can call `.lazy.scan(_:_:)` on any
/// sequence to create a lazily computed scan. The resulting `LazyScanSequence`
/// is itself lazy, too, so further sequence operations also defer computation.
///
/// The explicit permission to implement operations lazily applies 
/// only in contexts where the sequence is statically known to conform to
/// `LazySequenceProtocol`. In the following example, because the extension 
/// applies only to `Sequence`, side-effects such as the accumulation of
/// `result` are never unexpectedly dropped or deferred:
///
///     extension Sequence where Element == Int {
///         func sum() -> Int {
///             var result = 0
///             _ = self.map { result += $0 }
///             return result
///         }
///     }
///
/// Don't actually use `map` for this purpose, however, because it creates 
/// and discards the resulting array. Instead, use `reduce` for summing 
/// operations, or `forEach` or a `for`-`in` loop for operations with side 
/// effects.
public protocol LazySequenceProtocol: Sequence {
  /// A `Sequence` that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// - See also: `elements`
  associatedtype Elements: Sequence = Self where Elements.Element == Element

  /// A sequence containing the same elements as this one, possibly with
  /// a simpler type.
  ///
  /// When implementing lazy operations, wrapping `elements` instead of `self`
  /// can prevent result types from growing an extra `LazySequence` layer.
  ///
  /// Note: this property need not be implemented by conforming types,
  /// it has a default implementation in a protocol extension that
  /// just returns `self`.
  var elements: Elements { get }
}

/// When there's no special associated `Elements` type, the `elements`
/// property is provided.
extension LazySequenceProtocol where Elements == Self {
  /// Identical to `self`.
  @inlinable // protocol-only
  public var elements: Self { return self }
}

extension LazySequenceProtocol {
  @inlinable // protocol-only
  public var lazy: LazySequence<Elements> {
    return elements.lazy
  }
}

extension LazySequenceProtocol where Elements: LazySequenceProtocol {
  @inlinable // protocol-only
  public var lazy: Elements {
    return elements
  }
}

/// A sequence containing the same elements as a `Base` sequence, but
/// on which some operations such as `map` and `filter` are
/// implemented lazily.
///
/// - See also: `LazySequenceProtocol`
@frozen // lazy-performance
public struct LazySequence<Base: Sequence> {
  @usableFromInline
  internal var _base: Base

  /// Creates a sequence that has the same elements as `base`, but on
  /// which some operations such as `map` and `filter` are implemented
  /// lazily.
  @inlinable // lazy-performance
  internal init(_base: Base) {
    self._base = _base
  }
}

extension LazySequence: BitwiseCopyable where Base: BitwiseCopyable {}

extension LazySequence: Sendable where Base: Sendable {}

extension LazySequence: Sequence {
  public typealias Element = Base.Element
  public typealias Iterator = Base.Iterator

  @inlinable
  public __consuming func makeIterator() -> Iterator {
    return _base.makeIterator()
  }
  
  @inlinable // lazy-performance
  public var underestimatedCount: Int {
    return _base.underestimatedCount
  }

  @inlinable // lazy-performance
  @discardableResult
  public __consuming func _copyContents(
    initializing buf: UnsafeMutableBufferPointer<Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Element>.Index) {
    return unsafe _base._copyContents(initializing: buf)
  }

  @inlinable // lazy-performance
  public func _customContainsEquatableElement(_ element: Element) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  @inlinable // generic-performance
  public __consuming func _copyToContiguousArray() -> ContiguousArray<Element> {
    return _base._copyToContiguousArray()
  }
}

extension LazySequence: LazySequenceProtocol {
  public typealias Elements = Base

  /// The `Base` (presumably non-lazy) sequence from which `self` was created.
  @inlinable // lazy-performance
  public var elements: Elements { return _base }
}

extension Sequence {
  /// A sequence containing the same elements as this sequence,
  /// but on which some operations, such as `map` and `filter`, are
  /// implemented lazily.
  @inlinable // protocol-only
  public var lazy: LazySequence<Self> {
    return LazySequence(_base: self)
  }
}
