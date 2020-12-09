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
  public func compactMap<ElementOfResult>(_ transform: @escaping (Element) async -> ElementOfResult?) -> AsyncCompactMapSequence<Self, ElementOfResult> {
    return AsyncCompactMapSequence(self, transform: transform)
  }
  
  public func compactMap<ElementOfResult>(_ transform: @escaping (Element) async throws -> ElementOfResult?) -> AsyncTryCompactMapSequence<Self, ElementOfResult> {
    return AsyncTryCompactMapSequence(self, transform: transform)
  }
}

public struct AsyncCompactMapSequence<Upstream, ElementOfResult>: AsyncSequence where Upstream: AsyncSequence, Upstream.Element == ElementOfResult? {
  public typealias Element = ElementOfResult
  public typealias AsyncIterator = Iterator
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    let transform: (Upstream.Element) async -> ElementOfResult?
    
    init(_ upstreamIterator: Upstream.AsyncIterator, transform: @escaping (Upstream.Element) async -> ElementOfResult?) {
      self.upstreamIterator = upstreamIterator
      self.transform = transform
    }
    
    public mutating func next() async throws /*rethrows*/ -> ElementOfResult? {
      while true {
        guard let item = await try upstreamIterator?.next() else {
          return nil
        }
        if let transformed = await transform(item) {
          return transformed
        }
      }
    }
    
    public mutating func cancel() {
      upstreamIterator?.cancel()
      upstreamIterator = nil
    }
  }
  
  public let upstream: Upstream
  public let transform: (Upstream.Element) async -> ElementOfResult?
  
  public init(_ upstream: Upstream, transform: @escaping (Upstream.Element) async -> ElementOfResult?) {
    self.upstream = upstream
    self.transform = transform
  }
  
  public func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), transform: transform)
  }
}

public struct AsyncTryCompactMapSequence<Upstream, ElementOfResult>: AsyncSequence where Upstream: AsyncSequence, Upstream.Element == ElementOfResult? {
  public typealias Element = ElementOfResult
  public typealias AsyncIterator = Iterator
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    let transform: (Upstream.Element) async throws -> ElementOfResult?
    
    init(_ upstreamIterator: Upstream.AsyncIterator, transform: @escaping (Upstream.Element) async throws -> ElementOfResult?) {
      self.upstreamIterator = upstreamIterator
      self.transform = transform
    }
    
    public mutating func next() async throws /*rethrows*/ -> ElementOfResult? {
      while true {
        guard let item = await try upstreamIterator?.next() else {
          return nil
        }
        do {
          if let transformed = await try transform(item) {
            return transformed
          }
        } catch {
          upstreamIterator?.cancel()
          upstreamIterator = nil
          throw error
        }
      }
    }
    
    public mutating func cancel() {
      upstreamIterator?.cancel()
      upstreamIterator = nil
    }
  }
  
  public let upstream: Upstream
  public let transform: (Upstream.Element) async throws -> ElementOfResult?
  
  public init(_ upstream: Upstream, transform: @escaping (Upstream.Element) async throws -> ElementOfResult?) {
    self.upstream = upstream
    self.transform = transform
  }
  
  public func makeAsyncIterator() -> Iterator {
    return Iterator(upstream.makeAsyncIterator(), transform: transform)
  }
}
