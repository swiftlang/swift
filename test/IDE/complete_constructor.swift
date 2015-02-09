// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_1 | FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_1P | FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_1P
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_2 | FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_2P | FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_2P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_1 | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_1P | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_1P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_2 | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_2P | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_2P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_3P | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_3P

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_SELECTOR_1 | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_SELECTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1 | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1

func freeFunc() {}

//===---
//===--- Test that we complete calls to constructors (except for 'super'-based calls, which are tested separately).
//===---

struct ImplicitConstructors1 {
}

func testImplicitConstructors1() {
  ImplicitConstructors1#^IMPLICIT_CONSTRUCTORS_1^#
// IMPLICIT_CONSTRUCTORS_1: Begin completions, 1 items
// IMPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ()[#ImplicitConstructors1#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_1: End completions
}
func testImplicitConstructors1P() {
  ImplicitConstructors1(#^IMPLICIT_CONSTRUCTORS_1P^#
// IMPLICIT_CONSTRUCTORS_1P: Begin completions
// IMPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['('])[#ImplicitConstructors1#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_1P-NEXT: End completions
}

struct ImplicitConstructors2 {
  var instanceVar = 0
}

func testImplicitConstructors2() {
  ImplicitConstructors2#^IMPLICIT_CONSTRUCTORS_2^#
// IMPLICIT_CONSTRUCTORS_2: Begin completions, 2 items
// IMPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ({#instanceVar: Int#})[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ()[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2: End completions
}
func testImplicitConstructors2P() {
  ImplicitConstructors2(#^IMPLICIT_CONSTRUCTORS_2P^#
// IMPLICIT_CONSTRUCTORS_2P: Begin completions
// IMPLICIT_CONSTRUCTORS_2P-NEXT: Decl[Constructor]/CurrNominal: ['(']{#instanceVar: Int#})[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2P-NEXT: Decl[Constructor]/CurrNominal: ['('])[#ImplicitConstructors2#]{{; name=.+$}}
// IMPLICIT_CONSTRUCTORS_2P-NEXT: End completions
}

struct ExplicitConstructors1 {
  init() {}
  init(a: Int) {}
  init(a: Int, b: Float) {}
}

func testExplicitConstructors1() {
  ExplicitConstructors1#^EXPLICIT_CONSTRUCTORS_1^#
// EXPLICIT_CONSTRUCTORS_1: Begin completions, 3 items
// EXPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ()[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#})[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#}, {#b: Float#})[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1: End completions
}
func testExplicitConstructors1P() {
  ExplicitConstructors1(#^EXPLICIT_CONSTRUCTORS_1P^#
// EXPLICIT_CONSTRUCTORS_1P: Begin completions
// EXPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['('])[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['(']{#a: Int#})[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1P-NEXT: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}, {#b: Float#})[#ExplicitConstructors1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_1P-NEXT: End completions
}

ExplicitConstructors1#^EXPLICIT_CONSTRUCTORS_2^#

// EXPLICIT_CONSTRUCTORS_2: Begin completions, 3 items
// EXPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ()[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2-DAG: Decl[Constructor]/CurrNominal: ({#a: Int#}, {#b: Float#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2: End completions

ExplicitConstructors1(#^EXPLICIT_CONSTRUCTORS_2P^#

// EXPLICIT_CONSTRUCTORS_2P: Begin completions
// EXPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['('])[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['(']{#a: Int#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2P-DAG: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}, {#b: Float#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_2P: End completions


struct ExplicitConstructors3 {
  init() {}
  init(_ a: Int) {}
  init(a: Int, b: Float) {}
}

func testExplicitConstructors3P() {
  ExplicitConstructors3(#^EXPLICIT_CONSTRUCTORS_3P^#
// EXPLICIT_CONSTRUCTORS_3P: Begin completions
// EXPLICIT_CONSTRUCTORS_3P-DAG: Decl[Constructor]/CurrNominal: ['('])[#ExplicitConstructors3#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_3P-DAG: Decl[Constructor]/CurrNominal: ['(']{#(a): Int#})[#ExplicitConstructors3#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_3P-DAG: Decl[Constructor]/CurrNominal: ['(']{#a: Int#}, {#b: Float#})[#ExplicitConstructors3#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_3P-DAG: Decl[FreeFunction]/CurrModule: freeFunc()[#Void#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_3P: End completions
}


struct ExplicitConstructorsSelector1 {
  init(int a : Int) {}
  init(int a : Int, andFloat b : Float) {}
}

func testExplicitConstructorsSelector1() {
  ExplicitConstructorsSelector1#^EXPLICIT_CONSTRUCTORS_SELECTOR_1^#
// EXPLICIT_CONSTRUCTORS_SELECTOR_1: Begin completions, 2 items
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: Decl[Constructor]/CurrNominal: ({#int: Int#})[#ExplicitConstructorsSelector1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: Decl[Constructor]/CurrNominal: ({#int: Int#}, {#andFloat: Float#})[#ExplicitConstructorsSelector1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_1: End completions
}

struct ExplicitConstructorsSelector2 {
  init(noArgs _ : ()) {}
  init(_ a : Int) {}
  init(_ a : Int, withFloat b : Float) {}
  init(int a : Int, _ b : Float) {}
}

func testExplicitConstructorsSelector2() {
  ExplicitConstructorsSelector2#^EXPLICIT_CONSTRUCTORS_SELECTOR_2^#
// EXPLICIT_CONSTRUCTORS_SELECTOR_2: Begin completions, 4 items
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#noArgs: ()#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#Int#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#Int#}, {#withFloat: Float#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: Decl[Constructor]/CurrNominal: ({#int: Int#}, {#Float#})[#ExplicitConstructorsSelector2#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_2: End completions
}

class ExplicitConstructorsBase1 {
  init() {}
  init(a : Int) {}
}

class ExplicitConstructorsDerived1 : ExplicitConstructorsBase1 {
  init() {}
  init(a : Int) {}
}

func testExplicitConstructorsBaseDerived1() {
  ExplicitConstructorsDerived1#^EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1^#
}
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1: Begin completions, 2 items
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1-DAG: Decl[Constructor]/CurrNominal:  ()[#ExplicitConstructorsDerived1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1-DAG: Decl[Constructor]/CurrNominal:  ({#a: Int#})[#ExplicitConstructorsDerived1#]{{; name=.+$}}
// EXPLICIT_CONSTRUCTORS_BASE_DERIVED_1: End completions
