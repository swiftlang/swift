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
  public __consuming func prefix(
    while predicate: @escaping (Element) async -> Bool
  ) rethrows -> AsyncPrefixWhileSequence<Self> {
    return AsyncPrefixWhileSequence(self, predicate: predicate)
  }
}

@frozen
public struct AsyncPrefixWhileSequence<Upstream: AsyncSequence> {
  public let upstream: Upstream
  public let predicate: (Upstream.Element) async -> Bool

  @inlinable
  public init(
    _ upstream: Upstream, 
    predicate: @escaping (Upstream.Element) async -> Bool
  ) {
    self.upstream = upstream
    self.predicate = predicate
  }
}

extension AsyncPrefixWhileSequence: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var predicateHasFailed = false

    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator

    @usableFromInline
    let predicate: (Upstream.Element) async -> Bool

    @usableFromInline
    init(
      _ upstreamIterator: Upstream.AsyncIterator, 
      predicate: @escaping (Upstream.Element) async -> Bool
    ) {
      self.upstreamIterator = upstreamIterator
      self.predicate = predicate
    }

    @inlinable
    public mutating func next() async rethrows -> Upstream.Element? {
      if !predicateHasFailed, let nextElement = try await upstreamIterator.next() {
        if await predicate(nextElement) {
          return nextElement
        } else {
          predicateHasFailed = true
        }
      }
      return nil
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), predicate: predicate)
  }
}
