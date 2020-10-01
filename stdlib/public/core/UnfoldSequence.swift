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

/// Returns a sequence formed from `first` and repeated lazy applications of
/// `next`.
///
/// The first element in the sequence is always `first`, and each successive
/// element is the result of invoking `next` with the previous element. The
/// sequence ends when `next` returns `nil`. If `next` never returns `nil`, the
/// sequence is infinite.
///
/// This function can be used to replace many cases that were previously handled
/// using C-style `for` loops.
///
/// Example:
///
///     // Walk the elements of a tree from a node up to the root
///     for node in sequence(first: leaf, next: { $0.parent }) {
///       // node is leaf, then leaf.parent, then leaf.parent.parent, etc.
///     }
///
///     // Iterate over all powers of two (ignoring overflow)
///     for value in sequence(first: 1, next: { $0 * 2 }) {
///       // value is 1, then 2, then 4, then 8, etc.
///     }
///
/// - Parameter first: The first element to be returned from the sequence.
/// - Parameter next: A closure that accepts the previous sequence element and
///   returns the next element.
/// - Returns: A sequence that starts with `first` and continues with every
///   value returned by passing the previous element to `next`.
@inlinable // generic-performance
public func sequence<T>(first: T, next: @escaping (T) -> T?) -> UnfoldFirstSequence<T> {
  // The trivial implementation where the state is the next value to return
  // has the downside of being unnecessarily eager (it evaluates `next` one
  // step in advance). We solve this by using a boolean value to disambiguate
  // between the first value (that's computed in advance) and the rest.
  return sequence(state: (first, true), next: { (state: inout (T?, Bool)) -> T? in
    switch state {
    case (let value, true):
      state.1 = false
      return value
    case (let value?, _):
      let nextValue = next(value)
      state.0 = nextValue
      return nextValue
    case (nil, _):
      return nil
    }
  })
}

/// Returns a sequence formed from repeated lazy applications of `next` to a
/// mutable `state`.
///
/// The elements of the sequence are obtained by invoking `next` with a mutable
/// state. The same state is passed to all invocations of `next`, so subsequent
/// calls will see any mutations made by previous calls. The sequence ends when
/// `next` returns `nil`. If `next` never returns `nil`, the sequence is
/// infinite.
///
/// This function can be used to replace many instances of `AnyIterator` that
/// wrap a closure.
///
/// Example:
///
///     // Interleave two sequences that yield the same element type
///     sequence(state: (false, seq1.makeIterator(), seq2.makeIterator()), next: { iters in
///       iters.0 = !iters.0
///       return iters.0 ? iters.1.next() : iters.2.next()
///     })
///
/// - Parameter state: The initial state that will be passed to the closure.
/// - Parameter next: A closure that accepts an `inout` state and returns the
///   next element of the sequence.
/// - Returns: A sequence that yields each successive value from `next`.
@inlinable // generic-performance
public func sequence<T, State>(state: State, next: @escaping (inout State) -> T?)
  -> UnfoldSequence<T, State> {
  return UnfoldSequence(_state: state, _next: next)
}

/// The return type of `sequence(first:next:)`.
public typealias UnfoldFirstSequence<T> = UnfoldSequence<T, (T?, Bool)>

/// A sequence whose elements are produced via repeated applications of a
/// closure to some mutable state.
///
/// The elements of the sequence are computed lazily and the sequence may
/// potentially be infinite in length.
///
/// Instances of `UnfoldSequence` are created with the functions
/// `sequence(first:next:)` and `sequence(state:next:)`.
@frozen // generic-performance
public struct UnfoldSequence<Element, State>: Sequence, IteratorProtocol {
  @inlinable // generic-performance
  public mutating func next() -> Element? {
    guard !_done else { return nil }
    if let elt = _next(&_state) {
        return elt
    } else {
        _done = true
        return nil
    }
  }

  @inlinable // generic-performance
  internal init(_state: State, _next: @escaping (inout State) -> Element?) {
    self._state = _state
    self._next = _next
  }

  @usableFromInline // generic-performance
  internal var _state: State
  @usableFromInline // generic-performance
  internal let _next: (inout State) -> Element?
  @usableFromInline // generic-performance
  internal var _done = false
}
