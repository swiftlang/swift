// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_A | FileCheck %s -check-prefix=TEST_A
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_B | FileCheck %s -check-prefix=TEST_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_C | FileCheck %s -check-prefix=TEST_C
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_D | FileCheck %s -check-prefix=TEST_D

class A {
  init(int i: Int) {}
  init(double d: Double) {}
  convenience init(float f: Float) {
    self.init(Double(f))
  }
}

// TEST_A: Begin completions
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#A#]{{$}}
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#double: Double#})[#A#]{{$}}
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#float: Float#})[#A#]{{$}}
// TEST_A-NEXT: End completions

class B : A {
  var x = 0
  // A's subobject initializers are implicitly overridden.
  // A's complete object initializers are inherited.
}

// TEST_B: Begin completions
// TEST_B-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#B#]{{$}}
// TEST_B-NEXT: Decl[Constructor]/CurrNominal:      ({#double: Double#})[#B#]{{$}}
// TEST_B-NEXT: Decl[Constructor]/Super:            ({#float: Float#})[#A#]{{$}}
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
// TEST_C-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#C#]{{$}}
// TEST_C-NEXT: Decl[Constructor]/CurrNominal:      ({#c: C#})[#C#]{{$}}
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
// TEST_D-NEXT: Decl[Constructor]/CurrNominal:      ({#d: D#})[#D#]{{$}}
// TEST_D-NEXT: Decl[Constructor]/CurrNominal:      ({#int: Int#})[#D#]{{$}}
// TEST_D-NEXT: Decl[Constructor]/Super:            ({#c: C#})[#C#]{{$}}
// TEST_D-NEXT: End completions

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
}

