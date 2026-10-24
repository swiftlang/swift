// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -parse-as-library -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -O -parse-as-library -module-name Client -emit-sil -I %t %t/Client.swift | %FileCheck %s
// RUN: %empty-directory(%t/branch)
// RUN: %target-swift-frontend -O -parse-as-library -module-name Lib -emit-module -emit-module-path %t/branch/Lib.swiftmodule %t/LibBranch.swift
// RUN: %target-swift-frontend -O -parse-as-library -module-name Client -emit-sil -I %t/branch %t/Client.swift | %FileCheck %s

// A loop over an iterator, whose `next()` builds each element with a force-unwrapped failable initializer
// from a borrow of the iterator's reference, doesn't retain the reference for each element. This holds for
// a generic and a concrete iterator, and whether the initializer checks the element before or after
// copying the reference.

// CHECK-LABEL: sil [noinline] @$s6Client7iterateyy3Lib5OwnerCF :
// CHECK:       // Loop header
// CHECK-NOT:     strong_retain
// CHECK-NOT:     retain_value
// CHECK:       } // end sil function '$s6Client7iterateyy3Lib5OwnerCF'

// CHECK-LABEL: sil [noinline] @$s6Client15iterateConcreteyy3Lib5OwnerCF :
// CHECK:       // Loop header
// CHECK-NOT:     strong_retain
// CHECK-NOT:     retain_value
// CHECK:       } // end sil function '$s6Client15iterateConcreteyy3Lib5OwnerCF'

//--- Lib.swift

public enum Kind {
  case even, odd, other
}

public final class Owner {
  public init() {}
}

@frozen
public struct Node {
  public let owner: Owner
  public let id: Int

  @inlinable
  public init(owner: Owner, id: Int) {
    self.owner = owner
    self.id = id
  }

  public var kind: Kind { id & 1 == 0 ? .even : .odd }
}

public protocol NodeProtocol {
  init?(_ node: __shared Node)
}

@frozen
public struct Element: NodeProtocol {
  public let node: Node

  @inlinable
  public static func isKindOf(_ kind: Kind) -> Bool {
    switch kind {
    case .even, .odd: return true
    case .other: return false
    }
  }

  @inlinable
  public init?(_ node: __shared Node) {
    guard Self.isKindOf(node.kind) else { return nil }
    self.node = node
  }
}

@frozen
public struct NodeIterator<E: NodeProtocol>: IteratorProtocol {
  public let owner: Owner
  public let children: UnsafeBufferPointer<Int?>
  public var index: Int

  public init(owner: Owner, count: Int) {
    self.owner = owner
    let buffer = UnsafeMutableBufferPointer<Int?>.allocate(capacity: count)
    for i in 0..<count { buffer[i] = i }
    self.children = UnsafeBufferPointer(buffer)
    self.index = 0
  }

  @inlinable
  @inline(__always)
  public mutating func next() -> E? {
    let index = self.index
    guard index < children.count else { return nil }
    self.index = index + 1
    return E(Node(owner: owner, id: children[index]!))!
  }
}

@frozen
public struct ConcreteIterator: IteratorProtocol {
  public let owner: Owner
  public let children: UnsafeBufferPointer<Int?>
  public var index: Int

  public init(owner: Owner, count: Int) {
    self.owner = owner
    let buffer = UnsafeMutableBufferPointer<Int?>.allocate(capacity: count)
    for i in 0..<count { buffer[i] = i }
    self.children = UnsafeBufferPointer(buffer)
    self.index = 0
  }

  @inlinable
  @inline(__always)
  public mutating func next() -> Element? {
    let index = self.index
    guard index < children.count else { return nil }
    self.index = index + 1
    return Element(Node(owner: owner, id: children[index]!))!
  }
}

//--- LibBranch.swift

public final class Owner {
  public init() {}
}

@frozen
public struct Node {
  public let owner: Owner
  public let id: Int

  @inlinable
  public init(owner: Owner, id: Int) {
    self.owner = owner
    self.id = id
  }

  public var kind: Int { id & 1 }
}

public protocol NodeProtocol {
  init?(_ node: Node)
}

@frozen
public struct Element: NodeProtocol {
  public let node: Node

  @inlinable
  public init?(_ node: Node) {
    guard node.kind >= 0 else { return nil }
    self.node = node
  }
}

@frozen
public struct NodeIterator<E: NodeProtocol>: IteratorProtocol {
  public let owner: Owner
  public let children: UnsafeBufferPointer<Int?>
  public var index: Int

  public init(owner: Owner, count: Int) {
    self.owner = owner
    let buffer = UnsafeMutableBufferPointer<Int?>.allocate(capacity: count)
    for i in 0..<count { buffer[i] = i }
    self.children = UnsafeBufferPointer(buffer)
    self.index = 0
  }

  @inlinable
  @inline(__always)
  public mutating func next() -> E? {
    let index = self.index
    guard index < children.count else { return nil }
    self.index = index + 1
    return E(Node(owner: owner, id: children[index]!))!
  }
}

@frozen
public struct ConcreteIterator: IteratorProtocol {
  public let owner: Owner
  public let children: UnsafeBufferPointer<Int?>
  public var index: Int

  public init(owner: Owner, count: Int) {
    self.owner = owner
    let buffer = UnsafeMutableBufferPointer<Int?>.allocate(capacity: count)
    for i in 0..<count { buffer[i] = i }
    self.children = UnsafeBufferPointer(buffer)
    self.index = 0
  }

  @inlinable
  @inline(__always)
  public mutating func next() -> Element? {
    let index = self.index
    guard index < children.count else { return nil }
    self.index = index + 1
    return Element(Node(owner: owner, id: children[index]!))!
  }
}

//--- Client.swift

import Lib

@_optimize(none) @inline(never)
func use(_ element: Element) {}

@inline(never)
public func iterate(_ owner: Owner) {
  var iterator = NodeIterator<Element>(owner: owner, count: 8)
  while let element = iterator.next() {
    use(element)
  }
}

@inline(never)
public func iterateConcrete(_ owner: Owner) {
  var iterator = ConcreteIterator(owner: owner, count: 8)
  while let element = iterator.next() {
    use(element)
  }
}
