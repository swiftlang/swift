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
  public __consuming func flatMap<SegmentOfResult: AsyncSequence>(
    _ transform: @escaping (Element) async throws -> SegmentOfResult
  ) -> AsyncFailableFlatMapSequence<Self, SegmentOfResult> {
    return AsyncFailableFlatMapSequence(self, transform: transform)
  }
}

@frozen
public struct AsyncFailableFlatMapSequence<Upstream: AsyncSequence, SegmentOfResult: AsyncSequence> {
  public let upstream: Upstream
  public let transform: (Upstream.Element) async throws -> SegmentOfResult

  @inlinable
  public init(
    _ upstream: Upstream,
    transform: @escaping (Upstream.Element) async throws -> SegmentOfResult
  ) {
    self.upstream = upstream
    self.transform = transform
  }
}

extension AsyncFailableFlatMapSequence: AsyncSequence {
  public typealias Element = SegmentOfResult.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator

    @usableFromInline
    let transform: (Upstream.Element) async throws -> SegmentOfResult

    @usableFromInline
    var currentIterator: SegmentOfResult.AsyncIterator?

    @usableFromInline
    init(
      _ upstreamIterator: Upstream.AsyncIterator,
      transform: @escaping (Upstream.Element) async throws -> SegmentOfResult
    ) {
      self.upstreamIterator = upstreamIterator
      self.transform = transform
    }

    @inlinable
    public mutating func next() async throws -> SegmentOfResult.Element? {
      while true {
        if var iterator = currentIterator {
          guard let element = try await iterator.next() else {
            currentIterator = nil
            continue
          }
          currentIterator = iterator
          return element
        } else {
          guard let item = try await upstreamIterator.next() else {
            return nil
          }
          let segment = try await transform(item)
          var iterator = segment.makeAsyncIterator()
          guard let element = try await iterator.next() else {
            currentIterator = nil
            continue
          }
          currentIterator = iterator
          return element
        }
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), transform: transform)
  }
}
