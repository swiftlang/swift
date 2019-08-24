// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRAINT1 | %FileCheck %s -check-prefix=CONSTRAINT1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRAINT2 | %FileCheck %s -check-prefix=CONSTRAINT2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRAINT3 | %FileCheck %s -check-prefix=CONSTRAINT3

public protocol P1 {}
public protocol P2 {}
public struct Example<T: Any> {}

extension Example where T: P1 {
  func P1Method() {}
}

extension Example where T: P2 {
  func P2Method() {}
}

public struct S1 : P1 {}
public struct S2 : P2 {}

func foo1() {
  var I1 = Example<S1>()
  I1.#^CONSTRAINT1^#
}

// CONSTRAINT1: 	 Begin completions, 2 items
// CONSTRAINT1-NEXT: Keyword[self]/CurrNominal: self[#Example<S1>#]; name=self
// CONSTRAINT1-NEXT: Decl[InstanceMethod]/CurrNominal:   P1Method()[#Void#]; name=P1Method()
// CONSTRAINT1-NEXT: End completions

func foo2() {
  var I2 = Example<S2>()
  I2.#^CONSTRAINT2^#
}

// CONSTRAINT2:      Begin completions, 2 items
// CONSTRAINT2-NEXT: Keyword[self]/CurrNominal: self[#Example<S2>#]; name=self
// CONSTRAINT2-NEXT: Decl[InstanceMethod]/CurrNominal:   P2Method()[#Void#]; name=P2Method()
// CONSTRAINT2-NEXT: End completions

protocol MyIndexable {}
protocol MyCollection : MyIndexable {
  associatedtype Indices = MyDefaultIndices<Self>
  var indices: Indices { get }
}
struct MyDefaultIndices<Elements : MyIndexable> : MyCollection {}
extension MyCollection where Indices == MyDefaultIndices<Self> {
    var indices: MyDefaultIndices<Self> { return MyDefaultIndices() }
}
struct ConcreteCollection<Element> : MyCollection {}
func foo3() {
  ConcreteCollection<Int>().#^CONSTRAINT3^#
}
// CONSTRAINT3:      Begin completions, 2 items
// CONSTRAINT3-NEXT: Keyword[self]/CurrNominal: self[#ConcreteCollection<Int>#]; name=self
// CONSTRAINT3-NEXT: Decl[InstanceVar]/Super:            indices[#MyDefaultIndices<ConcreteCollection<Int>>#]; name=indices
// CONSTRAINT3-NEXT: End completions
