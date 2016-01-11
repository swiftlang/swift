//===--- LazySequence.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A sequence on which normally-eager operations such as `map` and
/// `filter` are implemented lazily.
///
/// Lazy sequences can be used to avoid needless storage allocation
/// and computation, because they use an underlying sequence for
/// storage and compute their elements on demand.  For example,
///
///     [1, 2, 3].lazy.map { $0 * 2 }
///
/// is a sequence containing { `2`, `4`, `6` }.  Each time an element
/// of the lazy sequence is accessed, an element of the underlying
/// array is accessed and transformed by the closure.
///
/// Sequence operations taking closure arguments, such as `map` and
/// `filter`, are normally eager: they use the closure immediately and
/// return a new array.  Using the `lazy` property gives the standard
/// library explicit permission to store the closure and the sequence
/// in the result, and defer computation until it is needed.
///
/// To add new lazy sequence operations, extend this protocol with
/// methods that return lazy wrappers that are themselves
/// `LazySequenceType`s.  For example, given an eager `scan`
/// method defined as follows
///
///     extension SequenceType {
///       /// Returns an array containing the results of
///       ///
///       ///   p.reduce(initial, combine: combine)
///       ///
///       /// for each prefix `p` of `self`, in order from shortest to
///       /// longest.  For example:
///       ///
///       ///     (1..<6).scan(0, combine: +) // [0, 1, 3, 6, 10, 15]
///       ///
///       /// - Complexity: O(N)
///       func scan<ResultElement>(
///         initial: ResultElement,
///         @noescape combine: (ResultElement, Generator.Element) -> ResultElement
///       ) -> [ResultElement] {
///         var result = [initial]
///         for x in self {
///           result.append(combine(result.last!, x))
///         }
///         return result
///       }
///     }
///
/// we can build a sequence that lazily computes the elements in the
/// result of `scan`:
///
///     struct LazyScanGenerator<Base: GeneratorType, ResultElement>
///       : GeneratorType {
///       mutating func next() -> ResultElement? {
///         return nextElement.map { result in
///           nextElement = base.next().map { combine(result, $0) }
///           return result
///         }
///       }
///       private var nextElement: ResultElement? // The next result of next().
///       private var base: Base                  // The underlying generator.
///       private let combine: (ResultElement, Base.Element) -> ResultElement
///     }
///     
///     struct LazyScanSequence<Base: SequenceType, ResultElement>
///       : LazySequenceType // Chained operations on self are lazy, too
///     {
///       func generate() -> LazyScanGenerator<Base.Generator, ResultElement> {
///         return LazyScanGenerator(
///           nextElement: initial, base: base.generate(), combine: combine)
///       }
///       private let initial: ResultElement
///       private let base: Base
///       private let combine:
///         (ResultElement, Base.Generator.Element) -> ResultElement
///     }
///
/// and finally, we can give all lazy sequences a lazy `scan` method:
///     
///     extension LazySequenceType {
///       /// Returns a sequence containing the results of
///       ///
///       ///   p.reduce(initial, combine: combine)
///       ///
///       /// for each prefix `p` of `self`, in order from shortest to
///       /// longest.  For example:
///       ///
///       ///     Array((1..<6).lazy.scan(0, combine: +)) // [0, 1, 3, 6, 10, 15]
///       ///
///       /// - Complexity: O(1)
///       func scan<ResultElement>(
///         initial: ResultElement,
///         combine: (ResultElement, Generator.Element) -> ResultElement
///       ) -> LazyScanSequence<Self, ResultElement> {
///         return LazyScanSequence(
///           initial: initial, base: self, combine: combine)
///       }
///     }
///
/// - See also: `LazySequence`, `LazyCollectionType`, `LazyCollection`
///
/// - Note: The explicit permission to implement further operations
///   lazily applies only in contexts where the sequence is statically
///   known to conform to `LazySequenceType`.  Thus, side-effects such
///   as the accumulation of `result` below are never unexpectedly
///   dropped or deferred:
///
///       extension SequenceType where Generator.Element == Int {
///         func sum() -> Int {
///           var result = 0
///           _ = self.map { result += $0 }
///           return result
///         }
///       }
///
///   [We don't recommend that you use `map` this way, because it
///   creates and discards an array. `sum` would be better implemented
///   using `reduce`].
public protocol LazySequenceType : SequenceType {
  /// A `SequenceType` that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// - See also: `elements`
  typealias Elements: SequenceType = Self

  /// A sequence containing the same elements as this one, possibly with
  /// a simpler type.
  ///
  /// When implementing lazy operations, wrapping `elements` instead
  /// of `self` can prevent result types from growing an extra
  /// `LazySequence` layer.  For example,
  ///
  /// _prext_ example needed
  ///
  /// Note: this property need not be implemented by conforming types,
  /// it has a default implementation in a protocol extension that
  /// just returns `self`.
  var elements: Elements {get} 
  
  var array: [Generator.Element] {get}
}

extension LazySequenceType {
  @available(*, unavailable, message="please construct an Array from your lazy sequence: Array(...)")
  public var array: [Generator.Element] { fatalError("unavailable") }
}

/// When there's no special associated `Elements` type, the `elements`
/// property is provided.
extension LazySequenceType where Elements == Self {
  /// Identical to `self`.
  public var elements: Self { return self }
}

/// A sequence containing the same elements as a `Base` sequence, but
/// on which some operations such as `map` and `filter` are
/// implemented lazily.
///
/// - See also: `LazySequenceType`
public struct LazySequence<Base : SequenceType>
  : LazySequenceType, _SequenceWrapperType {

  /// Creates a sequence that has the same elements as `base`, but on
  /// which some operations such as `map` and `filter` are implemented
  /// lazily.
  public init(_ base: Base) {
    self._base = base
  }
  
  public var _base: Base

  /// The `Base` (presumably non-lazy) sequence from which `self` was created.
  public var elements: Base { return _base }

  @available(*, unavailable, renamed="Base")
  public typealias S = Void
}

extension SequenceType {
  /// A sequence containing the same elements as a `Base` sequence,
  /// but on which some operations such as `map` and `filter` are
  /// implemented lazily.
  ///
  /// - See also: `LazySequenceType`, `LazySequence`
  public var lazy: LazySequence<Self> {
    return LazySequence(self)
  }
}

/// Avoid creating multiple layers of `LazySequence` wrapper.
/// Anything conforming to `LazySequenceType` is already lazy.
extension LazySequenceType {
  /// Identical to `self`.
  public var lazy: Self {
    return self
  }
}

@available(*, unavailable, message="Please use the sequence's '.lazy' property")
public func lazy<Base : SequenceType>(s: Base) -> LazySequence<Base> {
  fatalError("unavailable")
}

