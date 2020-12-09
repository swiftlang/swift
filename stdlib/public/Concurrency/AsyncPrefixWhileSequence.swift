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
  public func prefix(while predicate: @escaping (Element) async -> Bool) -> AsyncPrefixWhileSequence<Self> {
    return AsyncPrefixWhileSequence(self, predicate: predicate)
  }
  
  public func prefix(while predicate: @escaping (Element) async throws -> Bool) -> AsyncTryPrefixWhileSequence<Self> {
    return AsyncTryPrefixWhileSequence(self, predicate: predicate)
  }
}

public struct AsyncPrefixWhileSequence<Upstream>: AsyncSequence where Upstream: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    var predicate: (Element) async -> Bool
    
    init(_ upstreamIterator: Upstream.AsyncIterator, predicate: @escaping (Element) async -> Bool) {
      self.upstreamIterator = upstreamIterator
      self.predicate = predicate
    }
    
    public mutating func next() async throws /*rethrows*/ -> Element? {
      guard let item = await try upstreamIterator?.next() else {
        return nil
      }
      guard await predicate(item) else {
        upstreamIterator?.cancel()
        upstreamIterator = nil
        return nil
      }
      return item
    }
    
    public mutating func cancel() {
      upstreamIterator?.cancel()
      upstreamIterator = nil
    }
  }
  
  public let upstream: Upstream
  public let predicate: (Element) async -> Bool
  
  public init(_ upstream: Upstream, predicate: @escaping (Element) async -> Bool) {
    self.upstream = upstream
    self.predicate = predicate
  }
  
  public func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), predicate: predicate)
  }
}

public struct AsyncTryPrefixWhileSequence<Upstream>: AsyncSequence where Upstream: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    var predicate: (Element) async throws -> Bool
    
    init(_ upstreamIterator: Upstream.AsyncIterator, predicate: @escaping (Element) async throws -> Bool) {
      self.upstreamIterator = upstreamIterator
      self.predicate = predicate
    }
    
    public mutating func next() async throws -> Element? {
      guard let item = await try upstreamIterator?.next() else {
        return nil
      }
      guard await try predicate(item) else {
        upstreamIterator?.cancel()
        upstreamIterator = nil
        return nil
      }
      return item
    }
    
    public mutating func cancel() {
      upstreamIterator?.cancel()
      upstreamIterator = nil
    }
  }
  
  public let upstream: Upstream
  public let predicate: (Element) async throws -> Bool
  
  public init(_ upstream: Upstream, predicate: @escaping (Element) async throws -> Bool) {
    self.upstream = upstream
    self.predicate = predicate
  }
  
  public func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), predicate: predicate)
  }
}
