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
  public __consuming func flatMap<SegmentOfResult: AsyncSequence>(
    _ transform: @escaping (Element) async throws -> SegmentOfResult
  ) -> AsyncThrowingFlatMapSequence<Self, SegmentOfResult> {
    return AsyncThrowingFlatMapSequence(self, transform: transform)
  }
}

public struct AsyncThrowingFlatMapSequence<Base: AsyncSequence, SegmentOfResult: AsyncSequence> {
  @usableFromInline
  let base: Base

  @usableFromInline
  let transform: (Base.Element) async throws -> SegmentOfResult

  @usableFromInline
  init(
    _ base: Base,
    transform: @escaping (Base.Element) async throws -> SegmentOfResult
  ) {
    self.base = base
    self.transform = transform
  }
}

extension AsyncThrowingFlatMapSequence: AsyncSequence {
  public typealias Element = SegmentOfResult.Element
  public typealias AsyncIterator = Iterator

  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator

    @usableFromInline
    let transform: (Base.Element) async throws -> SegmentOfResult

    @usableFromInline
    var currentIterator: SegmentOfResult.AsyncIterator?

    @usableFromInline
    init(
      _ baseIterator: Base.AsyncIterator,
      transform: @escaping (Base.Element) async throws -> SegmentOfResult
    ) {
      self.baseIterator = baseIterator
      self.transform = transform
    }

    @inlinable
    public mutating func next() async throws -> SegmentOfResult.Element? {
      while true {
        if var iterator = currentIterator {
          guard let element = try await iterator.next() else {
            currentIterator = nil
            continue
          }
          currentIterator = iterator
          return element
        } else {
          guard let item = try await baseIterator.next() else {
            return nil
          }
          let segment = try await transform(item)
          var iterator = segment.makeAsyncIterator()
          guard let element = try await iterator.next() else {
            currentIterator = nil
            continue
          }
          currentIterator = iterator
          return element
        }
      }
    }
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(base.makeAsyncIterator(), transform: transform)
  }
}
