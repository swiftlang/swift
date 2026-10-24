// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -parse-as-library -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -O -parse-as-library -module-name Client -emit-sil -I %t %t/Client.swift | %FileCheck %s

// A loop over a generic iterator from another module, whose `next()` returns
// its `Optional` element indirectly in its generic form, doesn't retain or
// release the iterator's reference for each element it passes on as
// guaranteed.

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
  init(casting node: Node)
}

@frozen
public struct Element: NodeProtocol {
  public let node: Node

  @inlinable
  public init(casting node: Node) {
    precondition(node.kind >= 0)
    self.node = node
  }
}

@frozen
public struct NodeIterator<E: NodeProtocol>: IteratorProtocol {
  public let owner: Owner
  public let children: UnsafeBufferPointer<Int?>
  public var index: Int

  public init(owner: Owner, children: UnsafeBufferPointer<Int?>) {
    self.owner = owner
    self.children = children
    self.index = 0
  }

  @inlinable
  @inline(__always)
  public mutating func next() -> E? {
    let index = self.index
    guard index < children.count else { return nil }
    self.index = index + 1
    return E(casting: Node(owner: owner, id: children[index]!))
  }
}

//--- Client.swift

import Lib

@_optimize(none) @inline(never)
func use(_ element: Element) {}

// CHECK-LABEL: sil [noinline] @$s6Client7iterateyy3Lib5OwnerC_SRySiSgGtF :
// CHECK:       bb0(
// CHECK:         apply
// CHECK:       // Loop header
// CHECK-NOT:     strong_retain
// CHECK-NOT:     retain_value
// CHECK:       } // end sil function '$s6Client7iterateyy3Lib5OwnerC_SRySiSgGtF'
@inline(never)
public func iterate(_ owner: Owner, _ children: UnsafeBufferPointer<Int?>) {
  var iterator = NodeIterator<Element>(owner: owner, children: children)
  while let element = iterator.next() {
    use(element)
  }
}
