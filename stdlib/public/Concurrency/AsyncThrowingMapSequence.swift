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
  public __consuming func map<Transformed>(
    _ transform: @escaping (Element) async throws -> Transformed
  ) -> AsyncThrowingMapSequence<Self, Transformed> {
    return AsyncThrowingMapSequence(self, transform: transform)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncThrowingMapSequence<Base: AsyncSequence, Transformed> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let transform: (Base.Element) async throws -> Transformed

  @usableFromInline
  init(
    _ base: Base, 
    transform: @escaping (Base.Element) async throws -> Transformed
  ) {
    self.base = base
    self.transform = transform
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncThrowingMapSequence: AsyncSequence {
  public typealias Element = Transformed
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let transform: (Base.Element) async throws -> Transformed

    @usableFromInline
    var finished = false

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator, 
      transform: @escaping (Base.Element) async throws -> Transformed
    ) {
      self.baseIterator = baseIterator
      self.transform = transform
    }

    @inlinable
    public mutating func next() async throws -> Transformed? {
      guard !finished, let element = try await baseIterator.next() else {
        return nil
      }
      do {
        return try await transform(element)
      } catch {
        finished = true
        throw error   
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), transform: transform)
  }
}
