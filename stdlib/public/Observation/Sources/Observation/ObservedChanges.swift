//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import _Concurrency

@available(SwiftStdlib 5.9, *)
public struct ObservedChange<Subject: Observable>: @unchecked Sendable {
  private let _subject: Subject
  private var properties: TrackedProperties<Subject>
  
  init(subject: Subject, properties: TrackedProperties<Subject>) {
    self._subject = subject
    self.properties = properties
  }
  
  internal mutating func insert(_ keyPath: PartialKeyPath<Subject>) {
    properties.insert(keyPath)
  }
  
  public func contains(_ member: PartialKeyPath<Subject>) -> Bool {
    properties.contains(member)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedChange where Subject: Sendable {
  public var subject: Subject { _subject }
}

@available(SwiftStdlib 5.9, *)
public struct ObservedChanges<Subject: Observable, Isolation: Actor> {
  let context: ObservationRegistrar<Subject>.Context
  let properties: TrackedProperties<Subject>
  let isolation: Isolation
  
  init(
    _ context: ObservationRegistrar<Subject>.Context,
    properties: TrackedProperties<Subject>,
    isolation: Isolation
  ) {
    self.context = context
    self.properties = properties
    self.isolation = isolation
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedChanges: AsyncSequence {
  public typealias Element = ObservedChange<Subject>
  
  public struct Iterator: AsyncIteratorProtocol {
    final class Iteration {
      let context: ObservationRegistrar<Subject>.Context
      let id: Int
      
      init(
        _ context: ObservationRegistrar<Subject>.Context,
        properties: TrackedProperties<Subject>
      ) {
        self.context = context
        self.id = context.register(.transactions, properties: properties)
      }
      
      deinit {
        context.unregister(id)
      }
    }
    
    let iteration: Iteration
    let properties: TrackedProperties<Subject>
    let isolation: Isolation
    
    init(
      _ context: ObservationRegistrar<Subject>.Context,
      properties: TrackedProperties<Subject>,
      isolation: Isolation
    ) {
      self.iteration = Iteration(context, properties: properties)
      self.properties = properties
      self.isolation = isolation
      
    }
    
    public mutating func next() async -> Element? {
      await iteration.context.nextChange(
        for: properties,
        isolation: isolation,
        id: iteration.id
      )
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(context, properties: properties, isolation: isolation)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedChanges: @unchecked Sendable { }

@available(*, unavailable)
extension ObservedChanges.Iterator: Sendable { }
