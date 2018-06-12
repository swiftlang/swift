// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_A | %FileCheck %s -check-prefix=TEST_A
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_B | %FileCheck %s -check-prefix=TEST_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_C | %FileCheck %s -check-prefix=TEST_C
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_D | %FileCheck %s -check-prefix=TEST_D
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_D_DOT | %FileCheck %s -check-prefix=TEST_D_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_D_PAREN | %FileCheck %s -check-prefix=TEST_D_PAREN

class A {
  init(int i: Int) {}
  init(double d: Double) {}
  convenience init(float f: Float) {
    self.init(Double(f))
  }
}

// TEST_A: Begin completions
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#A#]{{; name=.+$}}
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#double: Double#})[#A#]{{; name=.+$}}
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#float: Float#})[#A#]{{; name=.+$}}
// TEST_A-NEXT: Keyword[self]/CurrNominal:          .self[#A.Type#]; name=self
// TEST_A-NEXT: End completions

class B : A {
  var x = 0
  // A's subobject initializers are implicitly overridden.
  // A's complete object initializers are inherited.
}

// TEST_B: Begin completions
// TEST_B-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#B#]{{; name=.+$}}
// TEST_B-NEXT: Decl[Constructor]/CurrNominal:      ({#double: Double#})[#B#]{{; name=.+$}}
// TEST_B-NEXT: Decl[Constructor]/Super:            ({#float: Float#})[#A#]{{; name=.+$}}
// TEST_B-NEXT: Keyword[self]/CurrNominal:          .self[#B.Type#]; name=self
// TEST_B-NEXT: End completions

class C : B {
  init(int i: Int) {
    super.init(int: i)
  }
  convenience init(c other: C) {
    self.init(int: 0)
  }
  // No initializers are inherited.
}

// TEST_C: Begin completions
// TEST_C-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#C#]{{; name=.+$}}
// TEST_C-NEXT: Decl[Constructor]/CurrNominal:      ({#c: C#})[#C#]{{; name=.+$}}
// TEST_C-NEXT: Keyword[self]/CurrNominal:          .self[#C.Type#]; name=self
// TEST_C-NEXT: End completions

class D : C {
  var y = 0
  // C's subobject initializers are implicitly overridden.
  // C's complete object initializers are inherited.
  // Initializers from A are not included in D's interface.

  convenience init(d other: D) {
    self.init(int: 0)
  }
}

// TEST_D: Begin completions
// TEST_D-NEXT: Decl[Constructor]/CurrNominal:      ({#d: D#})[#D#]{{; name=.+$}}
// TEST_D-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#D#]{{; name=.+$}}
// TEST_D-NEXT: Decl[Constructor]/Super:            ({#c: C#})[#C#]{{; name=.+$}}
// TEST_D-NEXT: Keyword[self]/CurrNominal:          .self[#D.Type#]; name=self
// TEST_D-NEXT: End completions

// TEST_D_DOT: Decl[Constructor]/CurrNominal:       init({#d: D#})[#D#]; name=init(d: D)
// TEST_D_DOT-NEXT: Decl[Constructor]/CurrNominal:  init({#int: Int#})[#D#]; name=init(int: Int)
// TEST_D_DOT-NEXT: Decl[Constructor]/Super:        init({#c: C#})[#C#]; name=init(c: C)

// TEST_D_PAREN: Decl[Constructor]/CurrNominal:       ['(']{#d: D#}[')'][#D#]; name=d: D
// TEST_D_PAREN-NEXT: Decl[Constructor]/CurrNominal:  ['(']{#int: Int#}[')'][#D#]; name=int: Int
// TEST_D_PAREN-NEXT: Decl[Constructor]/Super:        ['(']{#c: C#}[')'][#C#]; name=c: C

func testA() {
  A#^TEST_A^#
}

func testB() {
  B#^TEST_B^#
}

func testC() {
  C#^TEST_C^#
}

func testD() {
  D#^TEST_D^#
  D.#^TEST_D_DOT^#
  D(#^TEST_D_PAREN^#
}
