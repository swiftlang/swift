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

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  public __consuming func drop(
    while predicate: @escaping (Element) async -> Bool
  ) -> AsyncDropWhileSequence<Self> {
    AsyncDropWhileSequence(self, predicate: predicate)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncDropWhileSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let predicate: (Base.Element) async -> Bool

  @usableFromInline
  init(
    _ base: Base, 
    predicate: @escaping (Base.Element) async -> Bool
  ) {
    self.base = base
    self.predicate = predicate
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncDropWhileSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    var predicate: ((Base.Element) async -> Bool)?

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator, 
      predicate: @escaping (Base.Element) async -> Bool
    ) {
      self.baseIterator = baseIterator
      self.predicate = predicate
    }

    @inlinable
    public mutating func next() async rethrows -> Base.Element? {
      while let predicate = self.predicate {
        guard let element = try await baseIterator.next() else {
          return nil
        }
        if await predicate(element) == false {
          self.predicate = nil
          return element
        }
      }
      return try await baseIterator.next()
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), predicate: predicate)
  }
}
