// RUN: %target-typecheck-verify-swift

protocol P1  {
  func foo() -> Int
}

protocol P2 : P1 {
  func foo() -> Int
}

func f<C : P2> (_ elements: C) {
  var _: Int = elements.foo() // should not error
}

protocol _Collection  {
  associatedtype Index

  associatedtype _Element
  subscript(i: Index) -> _Element {get}
}

protocol Collection : _Collection, Sequence {
  subscript(i: Index) -> Iterator.Element {get}
}

protocol MutableCollection : Collection {
  subscript(i: Index) -> Iterator.Element {get set}
}

func insertionSort<
C: MutableCollection 
>(
  _ elements: inout C,
  i: C.Index
) {
  var _: C.Iterator.Element = elements[i] // should not error
}

// rdar://problem/21322215
protocol FactoryProtocol {
  associatedtype Item
}

protocol MyCollection : Swift.Collection {}

struct TestClass<
  Factory : FactoryProtocol,
  NodeCollection : MyCollection
  >
  where NodeCollection.Iterator.Element == Factory.Item {
  var flattenedNodes: NodeCollection

  func test() {
    let node1 = self.flattenedNodes[self.flattenedNodes.startIndex]
    let typecheck1: NodeCollection.Iterator.Element = node1
    _ = typecheck1
  }
}
