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

@available(SwiftStdlib 5.9, *)
public struct TrackedProperties<Root>: ExpressibleByArrayLiteral, @unchecked Sendable {
  public typealias Element = PartialKeyPath<Root>
  public typealias ArrayLiteralElement = PartialKeyPath<Root>
  
  internal var raw: Set<PartialKeyPath<Root>>
  
  internal init(raw keyPaths: Set<AnyKeyPath>) {
    self.raw = Set(keyPaths.compactMap { $0 as? PartialKeyPath<Root> })
  }
  
  public init(_ sequence: __owned some Sequence<PartialKeyPath<Root>>) {
    self.raw = Set(sequence)
  }
  
  public init() {
    self.raw = Set()
  }
  
  public init(arrayLiteral elements: PartialKeyPath<Root>...) {
    self.init(elements)
  }
  
  public func contains(_ member: PartialKeyPath<Root>) -> Bool {
    raw.contains(member)
  }
  
  @discardableResult
  public mutating func insert(
    _ newMember: __owned PartialKeyPath<Root>
  ) -> Bool {
    let (inserted, _) = raw.insert(newMember)
    return inserted
  }
  
  public mutating func remove(
    _ member: PartialKeyPath<Root>
  ) {
    raw.remove(member)
  }
}

@available(SwiftStdlib 5.9, *)
extension TrackedProperties where Root: Observable {
  public init(dependent: TrackedProperties<Root>) {
    self.init()
    for raw in dependent.raw {
      self.raw.formUnion(Root.dependencies(of: raw).raw)
    }
  }
}
