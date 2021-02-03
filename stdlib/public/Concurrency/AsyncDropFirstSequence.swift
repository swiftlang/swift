////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2021 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift

extension AsyncSequence {
  @inlinable
  public __consuming func dropFirst(
    _ k: Int = 1
  ) -> AsyncDropFirstSequence<Self> {
    return AsyncDropFirstSequence(self, dropping: k)
  }
}

@frozen
public struct AsyncDropFirstSequence<Upstream: AsyncSequence> {
  public let upstream: Upstream
  public let limit: Int
  
  @inlinable 
  public init(_ upstream: Upstream, dropping limit: Int) {
    precondition(limit >= 0, 
      "Can't drop a negative number of elements from an async sequence")
    self.upstream = upstream
    self.limit = limit
  }
}

extension AsyncDropFirstSequence: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator
    
    @usableFromInline
    var count: Int

    @usableFromInline
    init(_ upstreamIterator: Upstream.AsyncIterator, count: Int) {
      self.upstreamIterator = upstreamIterator
      self.count = count
    }

    @inlinable
    public mutating func next() async rethrows -> Upstream.Element? {
      var remainingToDrop = count
      while remainingToDrop > 0 {
        guard try await upstreamIterator.next() != nil else {
          return nil
        }
        remainingToDrop -= 1
      }
      count = 0
      return try await upstreamIterator.next()
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), count: limit)
  }
}

extension AsyncDropFirstSequence {
  @inlinable
  public __consuming func dropFirst(
    _ k: Int = 1
  ) -> AsyncDropFirstSequence<Upstream> {
    precondition(k >= 0, 
      "Can't drop a negative number of elements from an async sequence")
    return AsyncDropFirstSequence(upstream, dropping: limit + k)
  }
}

