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
  public func drop(while predicate: @escaping (Element) async -> Bool) -> AsyncDropWhileSequence<Self> {
    return AsyncDropWhileSequence(self, predicate: predicate)
  }
  
  public func drop(while predicate: @escaping (Element) async throws -> Bool) -> AsyncTryDropWhileSequence<Self> {
    return AsyncTryDropWhileSequence(self, predicate: predicate)
  }
}

public struct AsyncDropWhileSequence<Upstream>: AsyncSequence where Upstream: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    var predicate: ((Element) async -> Bool)?
    
    init(_ upstreamIterator: Upstream.AsyncIterator, predicate: @escaping (Element) async -> Bool) {
      self.upstreamIterator = upstreamIterator
      self.predicate = predicate
    }
    
    public mutating func next() async throws /*rethrows*/ -> Upstream.Element? {
      while true {
        guard let item = await try upstreamIterator?.next() else {
          return nil
        }
        if let predicate = self.predicate {
          if !(await predicate(item)) {
            self.predicate = nil
            return item
          }
        } else {
          return item
        }
      }
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

public struct AsyncTryDropWhileSequence<Upstream>: AsyncSequence where Upstream: AsyncSequence {
  public typealias Element = Upstream.Element
  public typealias AsyncIterator = Iterator
  
  public struct Iterator: AsyncIteratorProtocol {
    var upstreamIterator: Upstream.AsyncIterator?
    var predicate: ((Element) async throws -> Bool)?
    
    init(_ upstreamIterator: Upstream.AsyncIterator, predicate: @escaping (Element) async throws -> Bool) {
      self.upstreamIterator = upstreamIterator
      self.predicate = predicate
    }
    
    public mutating func next() async throws -> Upstream.Element? {
      while true {
        guard let item = await try upstreamIterator?.next() else {
          return nil
        }
        if let predicate = self.predicate {
          do {
            if !(await try predicate(item)) {
              self.predicate = nil
              return item
            }
          } catch {
            upstreamIterator?.cancel()
            upstreamIterator = nil
            throw error
          }
        } else {
          return item
        }
      }
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
