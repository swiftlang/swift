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
  @inlinable
  public __consuming func prefix(
    _ count: Int
  ) -> AsyncPrefixSequence<Self> {
    precondition(count >= 0,
      "Can't prefix a negative number of elements from an async sequence")
    return AsyncPrefixSequence(self, count: count)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncPrefixSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let count: Int

  @usableFromInline
  init(_ base: Base, count: Int) {
    self.base = base
    self.count = count
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncPrefixSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    var remaining: Int

    @usableFromInline
    init(_ baseIterator: Base.AsyncIterator, count: Int) {
      self.baseIterator = baseIterator
      self.remaining = count
    }

    @inlinable
    public mutating func next() async rethrows -> Base.Element? {
      if remaining != 0 {
        remaining &-= 1
        return try await baseIterator.next()
      } else {
        return nil
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), count: count)
  }
}
