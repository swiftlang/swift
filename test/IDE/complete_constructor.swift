// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_1 | FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IMPLICIT_CONSTRUCTORS_2 | FileCheck %s -check-prefix=IMPLICIT_CONSTRUCTORS_2

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_1 | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_1

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPLICIT_CONSTRUCTORS_SELECTOR_1 | FileCheck %s -check-prefix=EXPLICIT_CONSTRUCTORS_SELECTOR_1

//===---
//===--- Test that we complete calls to constructors (except for 'super'-based calls, which are tested separately).
//===---

struct ImplicitConstructors1 {
}

func testImplicitConstructors1() {
  ImplicitConstructors1#^IMPLICIT_CONSTRUCTORS_1^#
// IMPLICIT_CONSTRUCTORS_1: Begin completions, 2 items
// IMPLICIT_CONSTRUCTORS_1-DAG: SwiftDecl: ()[#ImplicitConstructors1#]{{$}}
// IMPLICIT_CONSTRUCTORS_1: End completions
}

struct ImplicitConstructors2 {
  var instanceVar : Int
}

func testImplicitConstructors2() {
  ImplicitConstructors2#^IMPLICIT_CONSTRUCTORS_2^#
// IMPLICIT_CONSTRUCTORS_2: Begin completions, 3 items
// IMPLICIT_CONSTRUCTORS_2-DAG: SwiftDecl: ({#instanceVar: Int#})[#ImplicitConstructors2#]{{$}}
// IMPLICIT_CONSTRUCTORS_2-DAG: SwiftDecl: ()[#ImplicitConstructors2#]{{$}}
// IMPLICIT_CONSTRUCTORS_2: End completions
}

struct ExplicitConstructors1 {
  init() {}
  init(a : Int) {}
}

func testExplicitConstructors1() {
  ExplicitConstructors1#^EXPLICIT_CONSTRUCTORS_1^#
// EXPLICIT_CONSTRUCTORS_1: Begin completions, 3 items
// EXPLICIT_CONSTRUCTORS_1-DAG: SwiftDecl: ()[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_1-DAG: SwiftDecl: ({#a: Int#})[#ExplicitConstructors1#]
// EXPLICIT_CONSTRUCTORS_1: End completions
}

struct ExplicitConstructorsSelector1 {
  init withInt(a : Int) {}
  init withInt(a : Int) andFloat(b : Float) {}
}

func testExplicitConstructorsSelector1() {
  ExplicitConstructorsSelector1#^EXPLICIT_CONSTRUCTORS_SELECTOR_1^#
// EXPLICIT_CONSTRUCTORS_SELECTOR_1: Begin completions, 3 items
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: SwiftDecl: ({#withInt: Int#})[#ExplicitConstructorsSelector1#]{{$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_1-DAG: SwiftDecl: ({#withInt: Int#}, {#andFloat: Float#})[#ExplicitConstructorsSelector1#]{{$}}
// EXPLICIT_CONSTRUCTORS_SELECTOR_1: End completions
}

struct ExplicitConstructorsSelector2 {
  init noArgs(_ : ()) {}
  init _(a : Int) {}
  init _(a : Int) withFloat(b : Float) {}
  init withInt(a : Int) _(b : Float) {}
}

func testExplicitConstructorsSelector2() {
  ExplicitConstructorsSelector2#^EXPLICIT_CONSTRUCTORS_SELECTOR_2^#
// EXPLICIT_CONSTRUCTORS_SELECTOR_2: Begin completions, 5 items
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: SwiftDecl: ({#noArgs: ()#})[#ExplicitConstructorsSelector2#]
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: SwiftDecl: ({#Int#})[#ExplicitConstructorsSelector2#]
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: SwiftDecl: ({#Int#}, {#withFloat: Float#})[#ExplicitConstructorsSelector2#]
// EXPLICIT_CONSTRUCTORS_SELECTOR_2-DAG: SwiftDecl: ({#withInt: Int#}, {#Float#})[#ExplicitConstructorsSelector2#]
// EXPLICIT_CONSTRUCTORS_SELECTOR_2: End completions
}

