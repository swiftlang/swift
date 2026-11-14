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
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public func filter(
    _ isIncluded: nonisolated(nonsending) @Sendable @escaping (Element) async throws(Failure) -> Bool
  ) -> some AsyncSequence<Element, Failure> {
    AsyncTypedFailureFilterSequence(self, isIncluded: isIncluded)
  }
  
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public func filter(
    _ isIncluded: nonisolated(nonsending) @Sendable @escaping (Element) async throws(Never) -> Bool
  ) -> some AsyncSequence<Element, Failure> {
    AsyncTypedFailureFilterSequence(self, isIncluded: isIncluded)
  }
  
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public func filter<TransformedFailure: Error>(
    _ isIncluded: nonisolated(nonsending) @Sendable @escaping (Element) async throws(TransformedFailure) -> Bool
  ) -> some AsyncSequence<Element, TransformedFailure> where Failure == Never {
    AsyncTypedFailureFilterSequence(AsyncMapFailureSequence(self, failureType: TransformedFailure.self), isIncluded: isIncluded)
  }
}

extension AsyncSequence where Self: Sendable, Element: Sendable {
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public func filter(
    _ isIncluded: nonisolated(nonsending) @Sendable @escaping (Element) async throws(Failure) -> Bool
  ) -> some AsyncSequence<Element, Failure> & Sendable {
    AsyncTypedFailureFilterSequence(self, isIncluded: isIncluded)
  }
  
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public func filter(
    _ isIncluded: nonisolated(nonsending) @Sendable @escaping (Element) async throws(Never) -> Bool
  ) -> some AsyncSequence<Element, Failure> & Sendable {
    AsyncTypedFailureFilterSequence(self, isIncluded: isIncluded)
  }
  
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public func filter<TransformedFailure: Error>(
    _ isIncluded: nonisolated(nonsending) @Sendable @escaping (Element) async throws(TransformedFailure) -> Bool
  ) -> some AsyncSequence<Element, TransformedFailure> & Sendable where Failure == Never {
    AsyncTypedFailureFilterSequence(AsyncMapFailureSequence(self, failureType: TransformedFailure.self), isIncluded: isIncluded)
  }
}

@available(SwiftStdlib 6.3, *)
@usableFromInline
struct AsyncTypedFailureFilterSequence<Base: AsyncSequence> {
  @usableFromInline
  let base: Base
  
  @usableFromInline
  let isolatedIsIncluded: @Sendable (isolated (any Actor)?, Base.Element) async throws(Base.Failure) -> Bool
  
  @inlinable
  init(_ base: Base, isIncluded: nonisolated(nonsending) @Sendable @escaping (Base.Element) async throws(Base.Failure) -> Bool) {
    self.base = base
    self.isolatedIsIncluded = { (isolation, element) async throws(Base.Failure) -> Bool in
      try await isIncluded(element)
    }
  }
  
  @inlinable
  init(_ base: Base, isIncluded: nonisolated(nonsending) @Sendable @escaping (Base.Element) async throws(Never) -> Bool) {
    self.base = base
    self.isolatedIsIncluded = { (isolation, element) async throws(Base.Failure) -> Bool in
      await isIncluded(element)
    }
  }
}

@available(SwiftStdlib 6.3, *)
extension AsyncTypedFailureFilterSequence: AsyncSequence {
  @usableFromInline
  typealias Element = Base.Element
  
  @usableFromInline
  typealias Failure = Base.Failure
  
  @available(SwiftStdlib 6.3, *)
  @usableFromInline
  struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var baseIterator: Base.AsyncIterator
    
    @usableFromInline
    let isolatedIsIncluded: @Sendable (isolated (any Actor)?, Base.Element) async throws(Base.Failure) -> Bool
    
    @usableFromInline
    var finished = false
    
    @inlinable
    init(_ baseIterator: Base.AsyncIterator, isolatedIsIncluded: @Sendable @escaping (isolated (any Actor)?, Base.Element) async throws(Base.Failure) -> Bool) {
      self.baseIterator = baseIterator
      self.isolatedIsIncluded = isolatedIsIncluded
    }
    
    @inlinable
    mutating func next(isolation actor: isolated (any Actor)?) async throws(Failure) -> Element? {
      while !finished {
        guard let element = try await baseIterator.next(isolation: actor) else {
          return nil
        }
        do {
          if try await isolatedIsIncluded(actor, element) {
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
  func makeAsyncIterator() -> Iterator {
    Iterator(base.makeAsyncIterator(), isolatedIsIncluded: isolatedIsIncluded)
  }
}

@available(SwiftStdlib 6.3, *)
extension AsyncTypedFailureFilterSequence: Sendable
  where Base: Sendable,
        Base.Element: Sendable { }

@available(*, unavailable)
extension AsyncTypedFailureFilterSequence.Iterator: @unchecked Sendable { }
