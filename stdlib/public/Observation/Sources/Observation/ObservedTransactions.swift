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
public struct ObservedTransactions<Subject: Observable, Delivery: Actor> {
  let context: ObservationRegistrar<Subject>.Context
  let properties: TrackedProperties<Subject>
  let isolation: Delivery
  
  init(
    _ context: ObservationRegistrar<Subject>.Context, 
    properties: TrackedProperties<Subject>, 
    isolation: Delivery
  ) {
    self.context = context
    self.properties = properties
    self.isolation = isolation
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedTransactions: AsyncSequence {
  public typealias Element = TrackedProperties<Subject>
  
  public struct Iterator: AsyncIteratorProtocol {
    let context: ObservationRegistrar<Subject>.Context
    let properties: TrackedProperties<Subject>
    let isolation: Delivery
    
    init(
      _ context: ObservationRegistrar<Subject>.Context, 
      properties: TrackedProperties<Subject>, 
      isolation: Delivery
    ) {
      self.context = context
      self.properties = properties
      self.isolation = isolation
    }
    
    public mutating func next() async -> Element? {
      await context.nextTransaction(for: properties, isolation: isolation)
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(context, properties: properties, isolation: isolation)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedTransactions: @unchecked Sendable { }

@available(*, unavailable)
extension ObservedTransactions.Iterator: Sendable { }
