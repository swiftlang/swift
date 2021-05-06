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
  public __consuming func filter(
    _ isIncluded: @escaping (Element) async throws -> Bool
  ) -> AsyncThrowingFilterSequence<Self> {
    return AsyncThrowingFilterSequence(self, isIncluded: isIncluded)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncThrowingFilterSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base
  
  @usableFromInline
  let isIncluded: (Element) async throws -> Bool

  @usableFromInline
  init(
    _ base: Base, 
    isIncluded: @escaping (Base.Element) async throws -> Bool
  ) {
    self.base = base
    self.isIncluded = isIncluded
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingFilterSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let isIncluded: (Base.Element) async throws -> Bool

    @usableFromInline
    var finished = false

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator,
      isIncluded: @escaping (Base.Element) async throws -> Bool
    ) {
      self.baseIterator = baseIterator
      self.isIncluded = isIncluded
    }

    @inlinable
    public mutating func next() async throws -> Base.Element? {
      while !finished {
        guard let element = try await baseIterator.next() else {
          return nil
        }
        do {
          if try await isIncluded(element) {
            return element
          }
        } catch {
          finished = true
          throw error
        }
      }

      return nil
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), isIncluded: isIncluded)
  }
}
