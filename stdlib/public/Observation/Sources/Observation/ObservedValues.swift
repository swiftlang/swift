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
public struct ObservedValues<Subject: Observable, Element: Sendable> {
  let context: ObservationRegistrar<Subject>.Context
  let keyPath: KeyPath<Subject, Element>
  
  init(
    _ context: ObservationRegistrar<Subject>.Context,
    keyPath: KeyPath<Subject, Element>
  ) {
    self.context = context
    self.keyPath = keyPath
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedValues: AsyncSequence {
  public struct Iterator: AsyncIteratorProtocol {
    final class Iteration {
      let context: ObservationRegistrar<Subject>.Context
      let id: Int
      
      init(
        _ context: ObservationRegistrar<Subject>.Context,
        properties: TrackedProperties<Subject>
      ) {
        self.context = context
        self.id = context.register(.changes, properties: properties)
      }
      
      deinit {
        context.unregister(id)
      }
    }
    let iteration: Iteration
    let keyPath: KeyPath<Subject, Element>
    let properties: TrackedProperties<Subject>
    
    
    init(
      _ context: ObservationRegistrar<Subject>.Context,
      keyPath: KeyPath<Subject, Element>
    ) {
      let properties = Subject.dependencies(of: keyPath)
      self.iteration = Iteration(context, properties: properties)
      self.keyPath = keyPath
      self.properties = properties
    }
    
    public mutating func next() async -> Element? {
      await iteration.context.nextValue(
        for: keyPath,
        properties: properties,
        id: iteration.id
      )
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(context, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedValues: @unchecked Sendable where Subject: Sendable { }

@available(*, unavailable)
extension ObservedValues.Iterator: Sendable { }
