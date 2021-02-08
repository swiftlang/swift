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
  public __consuming func dropFirst(
    _ k: Int = 1
  ) -> AsyncDropFirstSequence<Self> {
    return AsyncDropFirstSequence(self, dropping: k)
  }
}

@frozen
public struct AsyncDropFirstSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let limit: Int
  
  @usableFromInline 
  init(_ base: Base, dropping limit: Int) {
    precondition(limit >= 0, 
      "Can't drop a negative number of elements from an async sequence")
    self.base = base
    self.limit = limit
  }
}

extension AsyncDropFirstSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator
    
    @usableFromInline
    var count: Int

    @usableFromInline
    init(_ baseIterator: Base.AsyncIterator, count: Int) {
      self.baseIterator = baseIterator
      self.count = count
    }

    @inlinable
    public mutating func next() async rethrows -> Base.Element? {
      var remainingToDrop = count
      while remainingToDrop > 0 {
        guard try await baseIterator.next() != nil else {
          return nil
        }
        remainingToDrop -= 1
      }
      count = 0
      return try await baseIterator.next()
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), count: limit)
  }
}

extension AsyncDropFirstSequence {
  @inlinable
  public __consuming func dropFirst(
    _ k: Int = 1
  ) -> AsyncDropFirstSequence<Base> {
    precondition(k >= 0, 
      "Can't drop a negative number of elements from an async sequence")
    return AsyncDropFirstSequence(base, dropping: limit + k)
  }
}

