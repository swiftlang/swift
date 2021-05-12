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
  /// Omits elements from the base sequence until a given throwing closure returns
  /// `false`, after which it passes through all remaining elements.
  ///
  /// Use `drop(while:)` to omit elements from an asynchronous sequence until
  /// the element received meets a condition you specify. If the closure you
  /// provide throws an error, the sequence produces no elements and throws
  /// the error instead.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The predicate passed to the `drop(while:)`
  /// function throws an error if it encounters an even number, and otherwise
  /// returns `true` while it receives elements less than `5`. Because the
  /// predicate throws when it receives `2` from the base sequence, this example
  /// throws without ever printing anything.
  ///
  ///     for try await number in Counter(howHigh: 10)
  ///             .drop(while: {
  ///                 if $0 % 2 == 0 {
  ///                     throw EvenError()
  ///                 }
  ///                 return $0 < 5
  ///             })
  ///     {
  ///         print("\(number) ")
  ///     }
  ///     // No output; throws EvenError
  ///
  /// After the predicate returns `false`, the sequence never executes it again,
  /// and from then on the sequence passes through elements from its underlying
  /// sequence. A predicate that throws an error also never executes again.
  ///
  /// - Parameter predicate: A closure that takes an element as a parameter and
  ///   returns a Boolean value indicating whether to drop the element from the
  ///   modified sequence.
  /// - Returns: A sequence that skips over values until the provided closure
  ///   returns `false`.
  @inlinable
  public __consuming func drop(
    while predicate: @escaping (Element) async throws -> Bool
  ) -> AsyncThrowingDropWhileSequence<Self> {
    AsyncThrowingDropWhileSequence(self, predicate: predicate)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncThrowingDropWhileSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let predicate: (Base.Element) async throws -> Bool

  @usableFromInline
  init(
    _ base: Base, 
    predicate: @escaping (Base.Element) async throws -> Bool
  ) {
    self.base = base
    self.predicate = predicate
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingDropWhileSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let predicate: (Base.Element) async throws -> Bool

    @usableFromInline
    var finished = false

    @usableFromInline
    var doneDropping = false

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator, 
      predicate: @escaping (Base.Element) async throws -> Bool
    ) {
      self.baseIterator = baseIterator
      self.predicate = predicate
    }

    @inlinable
    public mutating func next() async throws -> Base.Element? {
      while !finished && !doneDropping {
        guard let element = try await baseIterator.next() else {
          return nil
        }
        do {
          if try await predicate(element) == false {
            doneDropping = true
            return element
          }
        } catch {
          finished = true
          throw error
        }
      }
      guard !finished else { 
        return nil
      }
      return try await baseIterator.next()
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), predicate: predicate)
  }
}
