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
  public __consuming func map<Transformed>(
    _ transform: @escaping (Element) async -> Transformed
  ) -> AsyncMapSequence<Self, Transformed> {
    return AsyncMapSequence(self, transform: transform)
  }
}

@frozen
public struct AsyncMapSequence<Upstream: AsyncSequence, Transformed> {
  public let upstream: Upstream
  public let transform: (Upstream.Element) async -> Transformed

  @inlinable
  public init(
    _ upstream: Upstream, 
    transform: @escaping (Upstream.Element) async -> Transformed
  ) {
    self.upstream = upstream
    self.transform = transform
  }
}

extension AsyncMapSequence: AsyncSequence {
  public typealias Element = Transformed
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator

    @usableFromInline
    let transform: (Upstream.Element) async -> Transformed

    @usableFromInline
    init(
      _ upstreamIterator: Upstream.AsyncIterator, 
      transform: @escaping (Upstream.Element) async -> Transformed
    ) {
      self.upstreamIterator = upstreamIterator
      self.transform = transform
    }

    @inlinable
    public mutating func next() async rethrows -> Transformed? {
      guard let element = try await upstreamIterator.next() else {
        return nil
      }
      return await transform(element)
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), transform: transform)
  }
}
