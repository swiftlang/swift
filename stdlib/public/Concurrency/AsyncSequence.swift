//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.5, *)
@rethrows
public protocol AsyncSequence {
  associatedtype AsyncIterator: AsyncIteratorProtocol where AsyncIterator.Element == Element
  associatedtype Element
  __consuming func makeAsyncIterator() -> AsyncIterator
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  public func reduce<Result>(
    _ initialResult: Result,
    _ nextPartialResult:
      (_ partialResult: Result, Element) async throws -> Result
  ) async rethrows -> Result {
    var accumulator = initialResult
    var iterator = makeAsyncIterator()
    while let element = try await iterator.next() {
      accumulator = try await nextPartialResult(accumulator, element)
    }
    return accumulator
  }

  @inlinable
  public func reduce<Result>(
    into initialResult: __owned Result,
    _ updateAccumulatingResult:
      (_ partialResult: inout Result, Element) async throws -> Void
  ) async rethrows -> Result {
    var accumulator = initialResult
    var iterator = makeAsyncIterator()
    while let element = try await iterator.next() {
      try await updateAccumulatingResult(&accumulator, element)
    }
    return accumulator
  }
}

@available(SwiftStdlib 5.5, *)
@inlinable
@inline(__always)
func _contains<Source: AsyncSequence>(
  _ self: Source,
  where predicate: (Source.Element) async throws -> Bool
) async rethrows -> Bool {
  for try await element in self {
    if try await predicate(element) {
      return true
    }
  }
  return false
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  public func contains(
    where predicate: (Element) async throws -> Bool
  ) async rethrows -> Bool {
    return try await _contains(self, where: predicate)
  }

  @inlinable
  public func allSatisfy(
    _ predicate: (Element) async throws -> Bool
  ) async rethrows -> Bool {
    return try await !contains { try await !predicate($0) }
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence where Element: Equatable {
  @inlinable
  public func contains(_ search: Element) async rethrows -> Bool {
    for try await element in self {
      if element == search {
        return true
      }
    }
    return false
  }
}

@available(SwiftStdlib 5.5, *)
@inlinable
@inline(__always)
func _first<Source: AsyncSequence>(
  _ self: Source,
  where predicate: (Source.Element) async throws -> Bool
) async rethrows -> Source.Element? {
  for try await element in self {
    if try await predicate(element) {
      return element
    }
  }
  return nil
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  public func first(
    where predicate: (Element) async throws -> Bool
  ) async rethrows -> Element? {
    return try await _first(self, where: predicate)
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  @warn_unqualified_access
  public func min(
    by areInIncreasingOrder: (Element, Element) async throws -> Bool
  ) async rethrows -> Element? {
    var it = makeAsyncIterator()
    guard var result = try await it.next() else { 
      return nil 
    }
    while let e = try await it.next() {
      if try await areInIncreasingOrder(e, result) { 
        result = e 
      }
    }
    return result
  }
  
  @inlinable
  @warn_unqualified_access
  public func max(
    by areInIncreasingOrder: (Element, Element) async throws -> Bool
  ) async rethrows -> Element? {
    var it = makeAsyncIterator()
    guard var result = try await it.next() else { 
      return nil 
    }
    while let e = try await it.next() {
      if try await areInIncreasingOrder(result, e) { 
        result = e 
      }
    }
    return result
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence where Element: Comparable {
  @inlinable
  @warn_unqualified_access
  public func min() async rethrows -> Element? {
    return try await self.min(by: <)
  }

  @inlinable
  @warn_unqualified_access
  public func max() async rethrows -> Element? {
    return try await self.max(by: <)
  }
}
