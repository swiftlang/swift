// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -parse-as-library -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -O -parse-as-library -module-name Client -emit-sil -I %t %t/Client.swift | %FileCheck %s

// A loop over a generic iterator from another module, whose elements are an enum that a failable
// initializer forms from the node's kind, and which the loop switches on, doesn't retain the iterator's
// reference for each element.

// CHECK-LABEL: sil [noinline] @$s6Client14iterateChoicesyy3Lib5OwnerCF :
// CHECK:       bb1
// CHECK-NOT:     strong_retain
// CHECK-NOT:     retain_value
// CHECK:       } // end sil function '$s6Client14iterateChoicesyy3Lib5OwnerCF'

//--- Lib.swift

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

  public var kind: Int { id % 3 }
}

public protocol NodeProtocol {
  init?(_ node: Node)
}

extension Node {
  @inlinable
  public func cast<S: NodeProtocol>(_: S.Type) -> S {
    return S(self)!
  }
}

@frozen
public struct A: NodeProtocol {
  public let node: Node
  @inlinable public static func isKindOf(_ kind: Int) -> Bool { kind == 0 }
  @inlinable public init(unsafeCasting node: Node) { self.node = node }
  @inlinable public init?(_ node: Node) {
    guard A.isKindOf(node.kind) else { return nil }
    self.init(unsafeCasting: node)
  }
}

@frozen
public struct B: NodeProtocol {
  public let node: Node
  @inlinable public static func isKindOf(_ kind: Int) -> Bool { kind == 1 }
  @inlinable public init(unsafeCasting node: Node) { self.node = node }
  @inlinable public init?(_ node: Node) {
    guard B.isKindOf(node.kind) else { return nil }
    self.init(unsafeCasting: node)
  }
}

@frozen
public enum Choice: NodeProtocol {
  case a(A)
  case b(B)

  @inlinable
  public init?(_ node: Node) {
    let kind = node.kind
    if A.isKindOf(kind) {
      self = .a(A(unsafeCasting: node))
    } else if B.isKindOf(kind) {
      self = .b(B(unsafeCasting: node))
    } else {
      return nil
    }
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
    for i in 0..<count { buffer[i] = 3 * i + (i & 1) }
    self.children = UnsafeBufferPointer(buffer)
    self.index = 0
  }

  @inlinable
  public mutating func next() -> E? {
    let index = self.index
    guard index < children.count else { return nil }
    self.index = index + 1
    return Node(owner: owner, id: children[index]!).cast(E.self)
  }
}

//--- Client.swift

import Lib

@_optimize(none) @inline(never)
func use(_ node: Node) {}

@inline(never)
public func iterateChoices(_ owner: Owner) {
  var iterator = NodeIterator<Choice>(owner: owner, count: 8)
  while let element = iterator.next() {
    switch element {
    case .a(let a): use(a.node)
    case .b(let b): use(b.node)
    }
  }
}
