// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | %FileCheck %s

class A {
  typealias T = Int
  enum E {
    case a
    case b
  }
}

class B : A {
  typealias T = String
  struct E {}
}

func foo() {
  _ = B.#^A^#
}

// CHECK-LABEL: Begin completions, 5 items
// CHECK-NEXT: Keyword[self]/CurrNominal:          self[#B.Type#]; name=self
// CHECK-NEXT: Keyword/CurrNominal:                Type[#B.Type#]; name=Type
// CHECK-NEXT: Decl[TypeAlias]/CurrNominal:        T[#String#]; name=T
// CHECK-NEXT: Decl[Struct]/CurrNominal:           E[#B.E#]; name=E
// CHECK-NEXT: Decl[Constructor]/CurrNominal:      init()[#B#]; name=init()
// CHECK-NEXT: End completions
