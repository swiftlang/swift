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
  public __consuming func prefix(
    while predicate: @escaping (Element) async throws -> Bool
  ) rethrows -> AsyncThrowingPrefixWhileSequence<Self> {
    return AsyncThrowingPrefixWhileSequence(self, predicate: predicate)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncThrowingPrefixWhileSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let predicate: (Base.Element) async throws -> Bool

  @usableFromInline
  init(
    _ base: Base, 
    predicate: @escaping (Base.Element) async throws -> Bool
  ) {
    self.base = base
    self.predicate = predicate
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingPrefixWhileSequence: AsyncSequence {
  public typealias Element = Base.Element
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var predicateHasFailed = false

    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let predicate: (Base.Element) async throws -> Bool

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator, 
      predicate: @escaping (Base.Element) async throws -> Bool
    ) {
      self.baseIterator = baseIterator
      self.predicate = predicate
    }

    @inlinable
    public mutating func next() async throws -> Base.Element? {
      if !predicateHasFailed, let nextElement = try await baseIterator.next() {
        do { 
          if try await predicate(nextElement) {
            return nextElement
          } else {
            predicateHasFailed = true
          }
        } catch {
          predicateHasFailed = true
          throw error
        }
      }
      return nil
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), predicate: predicate)
  }
}
