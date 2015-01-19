// RUN: %target-parse-verify-swift

protocol P1  {
  func foo() -> Int
}

protocol P2 : P1 {
  func foo() -> Int
}

func f<C : P2> (elements: C) {
  var x: Int = elements.foo() // should not error
}

protocol _CollectionType  {
  typealias Index

  typealias _Element
  subscript(i: Index) -> _Element {get}
}

protocol CollectionType : _CollectionType, SequenceType {
  subscript(i: Index) -> Generator.Element {get}
}

protocol MutableCollectionType : CollectionType {
  subscript(i: Index) -> Generator.Element {get set}
}

func insertionSort<
C: MutableCollectionType 
>(
  inout elements: C,
  i: C.Index
) {
  var x: C.Generator.Element = elements[i] // should not error
}
