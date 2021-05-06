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
    _ transform: @escaping (Element) async -> Transformed
  ) -> AsyncMapSequence<Self, Transformed> {
    return AsyncMapSequence(self, transform: transform)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncMapSequence<Base: AsyncSequence, Transformed> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let transform: (Base.Element) async -> Transformed

  @usableFromInline
  init(
    _ base: Base, 
    transform: @escaping (Base.Element) async -> Transformed
  ) {
    self.base = base
    self.transform = transform
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncMapSequence: AsyncSequence {
  public typealias Element = Transformed
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let transform: (Base.Element) async -> Transformed

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator, 
      transform: @escaping (Base.Element) async -> Transformed
    ) {
      self.baseIterator = baseIterator
      self.transform = transform
    }

    @inlinable
    public mutating func next() async rethrows -> Transformed? {
      guard let element = try await baseIterator.next() else {
        return nil
      }
      return await transform(element)
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), transform: transform)
  }
}
