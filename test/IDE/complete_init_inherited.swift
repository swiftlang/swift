// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_A | FileCheck %s -check-prefix=TEST_A
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_B | FileCheck %s -check-prefix=TEST_B
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_C | FileCheck %s -check-prefix=TEST_C
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_D | FileCheck %s -check-prefix=TEST_D

class A {
  init withInt(i: Int) {}
  init withDouble(d: Double) {}
  init withFloat(f: Float) -> Self {
    self.init(Double(f))
  }
}

// TEST_A: Begin completions
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#withInt: Int#})[#A#]{{$}}
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#withDouble: Double#})[#A#]{{$}}
// TEST_A-NEXT: Decl[Constructor]/CurrNominal:      ({#withFloat: Float#})[#A#]{{$}}
// TEST_A-NEXT: End completions

class B : A {
  var x = 0
  // A's subobject initializers are implicitly overridden.
  // A's complete object initializers are inherited.
}

// TEST_B: Begin completions
// TEST_B-NEXT: Decl[Constructor]/CurrNominal:      ({#withInt: Int#})[#B#]{{$}}
// TEST_B-NEXT: Decl[Constructor]/CurrNominal:      ({#withDouble: Double#})[#B#]{{$}}
// TEST_B-NEXT: Decl[Constructor]/Super:            ({#withFloat: Float#})[#A#]{{$}}
// TEST_B-NEXT: End completions

class C : B {
  init withInt(i: Int) {
    super.init(withInt: i)
  }
  init withC(other: C) -> Self {
    self.init(withInt: 0)
  }
  // No initializers are inherited.
}

// TEST_C: Begin completions
// TEST_C-NEXT: Decl[Constructor]/CurrNominal:      ({#withInt: Int#})[#C#]{{$}}
// TEST_C-NEXT: Decl[Constructor]/CurrNominal:      ({#withC: C#})[#C#]{{$}}
// TEST_C-NEXT: End completions

class D : C {
  var y = 0
  // C's subobject initializers are implicitly overridden.
  // C's complete object initializers are inherited.
  // Initializers from A are not included in D's interface.

  init withD(other: D) -> Self {
    self.init(withInt: 0)
  }
}

// TEST_D: Begin completions
// TEST_D-NEXT: Decl[Constructor]/CurrNominal:      ({#withD: D#})[#D#]{{$}}
// TEST_D-NEXT: Decl[Constructor]/CurrNominal:      ({#withInt: Int#})[#D#]{{$}}
// TEST_D-NEXT: Decl[Constructor]/Super:            ({#withC: C#})[#C#]{{$}}
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

