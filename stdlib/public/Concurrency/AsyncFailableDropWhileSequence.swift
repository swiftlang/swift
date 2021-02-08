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
  public __consuming func drop(
    while predicate: @escaping (Element) async throws -> Bool
  ) -> AsyncFailableDropWhileSequence<Self> {
    AsyncFailableDropWhileSequence(self, predicate: predicate)
  }
}

@frozen
public struct AsyncFailableDropWhileSequence<Upstream: AsyncSequence> {
  public let upstream: Upstream
  public let predicate: (Upstream.Element) async throws -> Bool

  @inlinable
  public init(
    _ upstream: Upstream, 
    predicate: @escaping (Upstream.Element) async throws -> Bool
  ) {
    self.upstream = upstream
    self.predicate = predicate
  }
}

extension AsyncFailableDropWhileSequence: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var upstreamIterator: Upstream.AsyncIterator

    @usableFromInline
    var predicate: ((Upstream.Element) async throws -> Bool)?

    @usableFromInline
    init(
      _ upstreamIterator: Upstream.AsyncIterator, 
      predicate: @escaping (Upstream.Element) async throws -> Bool
    ) {
      self.upstreamIterator = upstreamIterator
      self.predicate = predicate
    }

    @inlinable
    public mutating func next() async throws -> Upstream.Element? {
      while let predicate = self.predicate {
        guard let element = try await upstreamIterator.next() else {
          return nil
        }
        do {
          if try await predicate(element) == false {
            self.predicate = nil
            return element
          }
        } catch {
          self.predicate = nil
          throw error
        }
      }
      return try await upstreamIterator.next()
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), predicate: predicate)
  }
}
