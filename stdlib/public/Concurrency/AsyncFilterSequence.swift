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
  public __consuming func filter(
    _ isIncluded: @escaping (Element) async -> Bool
  ) -> AsyncFilterSequence<Self> {
    return AsyncFilterSequence(self, isIncluded: isIncluded)
  }
}

@frozen
public struct AsyncFilterSequence<Upstream: AsyncSequence> {
  @usableFromInline
  let upstream: Upstream

  @usableFromInline
  let isIncluded: (Element) async -> Bool

  @usableFromInline
  init(
    _ upstream: Upstream, 
    isIncluded: @escaping (Upstream.Element) async -> Bool
  ) {
    self.upstream = upstream
    self.isIncluded = isIncluded
  }
}

extension AsyncFilterSequence: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator

    @usableFromInline
    let isIncluded: (Upstream.Element) async -> Bool

    @usableFromInline
    init(
      _ upstreamIterator: Upstream.AsyncIterator,
      isIncluded: @escaping (Upstream.Element) async -> Bool
    ) {
      self.upstreamIterator = upstreamIterator
      self.isIncluded = isIncluded
    }

    @inlinable
    public mutating func next() async rethrows -> Upstream.Element? {
      while true {
        guard let element = try await upstreamIterator.next() else {
          return nil
        }
        if await isIncluded(element) {
          return element
        }
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), isIncluded: isIncluded)
  }
}
