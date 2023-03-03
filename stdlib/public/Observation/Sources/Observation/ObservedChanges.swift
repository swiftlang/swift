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
public struct ObservedChanges<Subject: Observable, Element: Sendable> {
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
extension ObservedChanges: AsyncSequence {
  public struct Iterator: AsyncIteratorProtocol {
    let context: ObservationRegistrar<Subject>.Context
    let keyPath: KeyPath<Subject, Element>
    let properties: TrackedProperties<Subject>
    
    init(
      _ context: ObservationRegistrar<Subject>.Context, 
      keyPath: KeyPath<Subject, Element>
    ) {
      self.context = context
      self.keyPath = keyPath
      properties = Subject.dependencies(of: keyPath)
    }
    
    public mutating func next() async -> Element? {
      await context.nextChange(to: keyPath, properties: properties)
    }
  }
  
  public func makeAsyncIterator() -> Iterator {
    Iterator(context, keyPath: keyPath)
  }
}

@available(SwiftStdlib 5.9, *)
extension ObservedChanges: @unchecked Sendable where Subject: Sendable { }

@available(*, unavailable)
extension ObservedChanges.Iterator: Sendable { }
