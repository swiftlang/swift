// RUN: %swift -parse %s -verify

protocol P1  {
  func foo() -> Int
}

protocol P2 : P1 {
  func foo() -> Int
}

func f<C : P2> (elements: C) {
  var x: Int = elements.foo() // should not error
}

protocol _Collection  {
  typealias IndexType

  typealias _Element
  subscript(i: IndexType) -> _Element {get}
}

protocol Collection : _Collection, Sequence {
  subscript(i: IndexType) -> GeneratorType.Element {get}
}

protocol MutableCollection : Collection {
  subscript(i: IndexType) -> GeneratorType.Element {get set}
}

func insertionSort<
C: MutableCollection 
>(
  inout elements: C,
  i: C.IndexType
) {
  var x: C.GeneratorType.Element = elements[i] // should not error
}
