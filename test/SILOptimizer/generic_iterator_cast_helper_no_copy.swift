// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -parse-as-library -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -O -parse-as-library -module-name Client -emit-sil -I %t %t/Client.swift | %FileCheck %s

// A loop over a generic iterator from another module, whose `next()` forms each element with an inlinable
// generic helper that force-unwraps a failable initializer, doesn't retain the iterator's reference for
// each element.

// CHECK-LABEL: sil [noinline] @$s6Client7iterateyy3Lib5OwnerCF :
// CHECK:       bb1
// CHECK-NOT:     strong_retain
// CHECK-NOT:     retain_value
// CHECK:       } // end sil function '$s6Client7iterateyy3Lib5OwnerCF'

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

  public var kind: Int { id & 1 }
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
func use(_ element: Element) {}

@inline(never)
public func iterate(_ owner: Owner) {
  var iterator = NodeIterator<Element>(owner: owner, count: 8)
  while let element = iterator.next() {
    use(element)
  }
}
