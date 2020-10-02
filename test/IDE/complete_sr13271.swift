// RUN: %swift-ide-test -code-completion -source-filename=%s -code-completion-token=A | %FileCheck %s --check-prefix=A
// RUN: %swift-ide-test -code-completion -source-filename=%s -code-completion-token=B | %FileCheck %s --check-prefix=B
// RUN: %swift-ide-test -code-completion -source-filename=%s -code-completion-token=D | %FileCheck %s --check-prefix=D

// https://bugs.swift.org/browse/SR-13271
// https://forums.swift.org/t/code-completion-enhancement-request/38677

enum AIdentifier {
  case a
}

enum BIdentifier {
  case b
}

struct X { }
struct Y { }

struct A <T> {
  private init(){}
  static func foo (arg: Bool) -> A<X> { A<X>() }
  static func bar (arg: Int) -> A<Y> { A<Y>() }
}

struct B {
  static var baz: B { B() }
}

func C<T>(_ identifier: AIdentifier, _ a: ()->A<T>) -> D<T> { }
func C(_ identifier: BIdentifier, _ b: ()->B) { }

struct D <T> {
    func sink (_ handler: @escaping (T)->()) { }
}

func test() {
  C(.a) {
    .#^A^#
  }
// A: Begin completions, 2 items
// A-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: foo({#arg: Bool#})[#A<X>#];
// A-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: bar({#arg: Int#})[#A<Y>#];
// A: End completions
}

func test() {
  C(.b) {
    .#^B^#
  }
// B: Begin completions, 2 items
// B-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: baz[#B#]; name=baz
// B-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init()[#B#]; name=init()
// B: End completions
}

func test() {
  C(.a) {
    .foo(arg: true)
  }
  .sink { value in
    value.#^D^#
  }
// D: Begin completions, 1 items
// D-DAG: Keyword[self]/CurrNominal:          self[#X#];
// D: End completions
}
