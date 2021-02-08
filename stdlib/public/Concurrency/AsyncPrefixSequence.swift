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
public struct AsyncPrefixSequence<Upstream: AsyncSequence> {
  @usableFromInline
  let upstream: Upstream

  @usableFromInline
  let maxLength: Int

  @usableFromInline
  init(_ upstream: Upstream, maxLength: Int) {
    self.upstream = upstream
    self.maxLength = maxLength
  }
}

extension AsyncPrefixSequence: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator

    @usableFromInline
    var remaining: Int

    @usableFromInline
    init(_ upstreamIterator: Upstream.AsyncIterator, maxLength: Int) {
      self.upstreamIterator = upstreamIterator
      self.remaining = maxLength
    }

    @inlinable
    public mutating func next() async rethrows -> Upstream.Element? {
      if remaining != 0 {
        remaining &-= 1
        return try await upstreamIterator.next()
      } else {
        return nil
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), maxLength: maxLength)
  }
}
