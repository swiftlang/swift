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

extension AsyncSequence {
  @inlinable
  public __consuming func prefix(
    _ maxLength: Int
  ) -> AsyncPrefixSequence<Self> {
    return AsyncPrefixSequence(self, maxLength: maxLength)
  }
}

@frozen
public struct AsyncPrefixSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let maxLength: Int

  @usableFromInline
  init(_ base: Base, maxLength: Int) {
    self.base = base
    self.maxLength = maxLength
  }
}

extension AsyncPrefixSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    var remaining: Int

    @usableFromInline
    init(_ baseIterator: Base.AsyncIterator, maxLength: Int) {
      self.baseIterator = baseIterator
      self.remaining = maxLength
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
    return Iterator(base.makeAsyncIterator(), maxLength: maxLength)
  }
}
