////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2020 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift

extension AsyncSequence {
  public func map<Transformed>(_ transform: @escaping (Element) async -> Transformed) -> AsyncMapSequence<Self, Transformed> {
    return AsyncMapSequence(self, transform: transform)
  }
  
  public func map<Transformed>(_ transform: @escaping (Element) async throws -> Transformed) -> AsyncTryMapSequence<Self, Transformed> {
    return AsyncTryMapSequence(self, transform: transform)
  }
}

public struct AsyncMapSequence<Upstream, Transformed>: AsyncSequence where Upstream: AsyncSequence {
  public typealias AsyncIterator = Iterator
  public typealias Element = Transformed
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    let transform: (Upstream.Element) async -> Transformed
    
    init(_ upstreamIterator: Upstream.AsyncIterator,
         transform: @escaping (Upstream.Element) async -> Transformed
    ) {
      self.upstreamIterator = upstreamIterator
      self.transform = transform
    }
    
    public mutating func next() async throws /*rethrows*/ -> Transformed? {
      guard let item = await try upstreamIterator?.next() else {
        return nil
      }
      return await transform(item)
    }
    
    public mutating func cancel() {
      upstreamIterator?.cancel()
      upstreamIterator = nil
    }
  }
  
  public let upstream: Upstream
  public let transform: (Upstream.Element) async -> Transformed
  
  public init(_ upstream: Upstream, transform: @escaping (Upstream.Element) async -> Transformed) {
    self.upstream = upstream
    self.transform = transform
  }
  
  public func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), transform: transform)
  }
}

public struct AsyncTryMapSequence<Upstream, Transformed>: AsyncSequence where Upstream: AsyncSequence {
  public typealias AsyncIterator = Iterator
  public typealias Element = Transformed
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    let transform: (Upstream.Element) async throws -> Transformed
    
    init(_ upstreamIterator: Upstream.AsyncIterator,
         transform: @escaping (Upstream.Element) async throws -> Transformed
    ) {
      self.upstreamIterator = upstreamIterator
      self.transform = transform
    }
    
    public mutating func next() async throws -> Transformed? {
      guard let item = await try upstreamIterator?.next() else {
        return nil
      }
      return await try transform(item)
    }
    
    public mutating func cancel() {
      upstreamIterator?.cancel()
      upstreamIterator = nil
    }
  }
  
  public let upstream: Upstream
  public let transform: (Upstream.Element) async throws -> Transformed
  
  public init(_ upstream: Upstream, transform: @escaping (Upstream.Element) async throws -> Transformed) {
    self.upstream = upstream
    self.transform = transform
  }
  
  public func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), transform: transform)
  }
}
