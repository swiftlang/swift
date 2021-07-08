//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  /// Creates an asynchronous sequence that concatenates the results of calling
  /// the given transformation with each element of this sequence.
  ///
  /// Use this method to receive a single-level asynchronous sequence when your
  /// transformation produces an asynchronous sequence for each element.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `5`. The transforming closure takes the received `Int`
  /// and returns a new `Counter` that counts that high. For example, when the
  /// transform receives `3` from the base sequence, it creates a new `Counter`
  /// that produces the values `1`, `2`, and `3`. The `flatMap(_:)` method
  /// "flattens" the resulting sequence-of-sequences into a single
  /// `AsyncSequence`.
  ///
  ///     let stream = Counter(howHigh: 5)
  ///         .flatMap { Counter(howHigh: $0) }
  ///     for await number in stream {
  ///         print("\(number)", terminator: " ")
  ///     }
  ///     // Prints: 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5
  ///
  /// - Parameter transform: A mapping closure. `transform` accepts an element
  ///   of this sequence as its parameter and returns an `AsyncSequence`.
  /// - Returns: A single, flattened asynchronous sequence that contains all
  ///   elements in all the asychronous sequences produced by `transform`.
 @inlinable
  public __consuming func flatMap<SegmentOfResult: AsyncSequence>(
    _ transform: @escaping (Element) async -> SegmentOfResult
  ) -> AsyncFlatMapSequence<Self, SegmentOfResult> {
    return AsyncFlatMapSequence(self, transform: transform)
  }
}

/// An asynchronous sequence that concatenates the results of calling a given
/// transformation with each element of this sequence.
@available(SwiftStdlib 5.5, *)
@frozen
public struct AsyncFlatMapSequence<Base: AsyncSequence, SegmentOfResult: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let transform: (Base.Element) async -> SegmentOfResult

  @inlinable
  init(
    _ base: Base,
    transform: @escaping (Base.Element) async -> SegmentOfResult
  ) {
    self.base = base
    self.transform = transform
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncFlatMapSequence: AsyncSequence {
  /// The type of element produced by this asynchronous sequence.
  ///
  /// The flat map sequence produces the type of element in the asynchronous
  /// sequence produced by the `transform` closure.
  public typealias Element = SegmentOfResult.Element
  /// The type of iterator that produces elements of the sequence.
  public typealias AsyncIterator = Iterator

  /// The iterator that produces elements of the flat map sequence.
  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let transform: (Base.Element) async -> SegmentOfResult

    @usableFromInline
    var currentIterator: SegmentOfResult.AsyncIterator?

    @usableFromInline
    var finished = false

    @inlinable
    init(
      _ baseIterator: Base.AsyncIterator,
      transform: @escaping (Base.Element) async -> SegmentOfResult
    ) {
      self.baseIterator = baseIterator
      self.transform = transform
    }

    /// Produces the next element in the flat map sequence.
    ///
    /// This iterator calls `next()` on its base iterator; if this call returns
    /// `nil`, `next()` returns `nil`. Otherwise, `next()` calls the
    /// transforming closure on the received element, takes the resulting
    /// asynchronous sequence, and creates an asynchronous iterator from it.
    /// `next()` then consumes values from this iterator until it terminates.
    /// At this point, `next()` is ready to receive the next value from the base
    /// sequence.
    @inlinable
    public mutating func next() async rethrows -> SegmentOfResult.Element? {
      while !finished {
        if var iterator = currentIterator {
          do {
            guard let element = try await iterator.next() else {
              currentIterator = nil
              continue
            }
            // restore the iterator since we just mutated it with next
            currentIterator = iterator
            return element
          } catch {
            finished = true
            throw error
          }
        } else {
          guard let item = try await baseIterator.next() else {
            finished = true
            return nil
          }
          do { 
            let segment = await transform(item)
            var iterator = segment.makeAsyncIterator()
            guard let element = try await iterator.next() else {
              currentIterator = nil
              continue
            }
            currentIterator = iterator
            return element
          } catch {
            finished = true
            throw error
          }
        }
      }
      return nil
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), transform: transform)
  }
}
