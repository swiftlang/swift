// RUN: %target-parse-verify-swift

protocol P1  {
  func foo() -> Int
}

protocol P2 : P1 {
  func foo() -> Int
}

func f<C : P2> (elements: C) {
  var _: Int = elements.foo() // should not error
}

protocol _CollectionType  {
  typealias Index

  typealias _Element
  subscript(i: Index) -> _Element {get}
}

protocol CollectionType : _CollectionType, SequenceType {
  subscript(i: Index) -> Iterator.Element {get}
}

protocol MutableCollectionType : CollectionType {
  subscript(i: Index) -> Iterator.Element {get set}
}

func insertionSort<
C: MutableCollectionType 
>(
  inout elements: C,
  i: C.Index
) {
  var _: C.Iterator.Element = elements[i] // should not error
}

// rdar://problem/21322215
protocol FactoryType {
  typealias Item
}

protocol MyCollectionType : Swift.CollectionType {}

struct TestClass<
  Factory: FactoryType,
  NodeCollection: MyCollectionType
  where
  NodeCollection.Iterator.Element == Factory.Item
> {
  var flattenedNodes: NodeCollection

  func test() {
    let node1 = self.flattenedNodes[self.flattenedNodes.startIndex]
    let typecheck1: NodeCollection.Iterator.Element = node1
    _ = typecheck1
  }
}
