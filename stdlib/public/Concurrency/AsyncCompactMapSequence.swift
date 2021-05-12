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
  public __consuming func compactMap<ElementOfResult>(
    _ transform: @escaping (Element) async -> ElementOfResult?
  ) -> AsyncCompactMapSequence<Self, ElementOfResult> {
    return AsyncCompactMapSequence(self, transform: transform)
  }
}

@available(SwiftStdlib 5.5, *)
public struct AsyncCompactMapSequence<Base: AsyncSequence, ElementOfResult> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let transform: (Base.Element) async -> ElementOfResult?

  @usableFromInline
  init(
    _ base: Base, 
    transform: @escaping (Base.Element) async -> ElementOfResult?
  ) {
    self.base = base
    self.transform = transform
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncCompactMapSequence: AsyncSequence {
  public typealias Element = ElementOfResult
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    public typealias Element = ElementOfResult

    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let transform: (Base.Element) async -> ElementOfResult?

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator, 
      transform: @escaping (Base.Element) async -> ElementOfResult?
    ) {
      self.baseIterator = baseIterator
      self.transform = transform
    }

    @inlinable
    public mutating func next() async rethrows -> ElementOfResult? {
      while true {
        guard let element = try await baseIterator.next() else {
          return nil
        }

        if let transformed = await transform(element) {
          return transformed
        }
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), transform: transform)
  }
}
