// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/55711
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
  C(.a) {
    ()
    return .#^A_MULTISTMT?check=A^#
  }
// A: Begin completions, 2 items
// A-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: foo({#arg: Bool#})[#A<X>#];
// A-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: bar({#arg: Int#})[#A<Y>#];
}

func test() {
  C(.b) {
    .#^B^#
  }
// B: Begin completions, 2 items
// B-DAG: Decl[StaticVar]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: baz[#B#]; name=baz
// B-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init()[#B#]; name=init()
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
}
