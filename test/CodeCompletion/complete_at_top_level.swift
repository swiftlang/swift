// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_1 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_1
// ZZZ: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_2 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_3 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_4 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_5 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_5

// Test code completion in top-level code.
//
// This test is not meant to test that we can correctly form all kinds of
// completion results in general; that should be tested elsewhere.

struct FooStruct {
  var instanceVar : Int

  // Add more stuff as needed.
}

var fooObject : FooStruct

//===--- Test code completions of expressions that can be typechecked.

// Although the parser can recover in most of these test cases, we resync it
// anyway to ensure that there parser recovery does not interfere with code
// completion.
func resyncParser1() {}

fooObject#^TYPE_CHECKED_EXPR_1^#
// TYPE_CHECKED_EXPR_1: Begin completions
// TYPE_CHECKED_EXPR_1-DAG: SwiftDecl: .instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_1: End completions

func resyncParser2() {}

// Test that we can code complete after a top-level var decl.
// FIXME: This is disabled because it is not working for unknown reason.
var _tmpVar1 : FooStruct

fooObject#^TYPE_CHECKED_EXPR_2^#
// TYPE_CHECKED_EXPR_2: Begin completions
// TYPE_CHECKED_EXPR_2-DAG: SwiftDecl: .instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_2: End completions

func resyncParser3() {}

fooObject#^TYPE_CHECKED_EXPR_3^#.bar
// TYPE_CHECKED_EXPR_3: Begin completions
// TYPE_CHECKED_EXPR_3-DAG: SwiftDecl: .instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_3: End completions

func resyncParser4() {}

fooObject.#^TYPE_CHECKED_EXPR_4^#
// TYPE_CHECKED_EXPR_4: Begin completions
// TYPE_CHECKED_EXPR_4-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_4: End completions

func resyncParser5() {}

fooObject.#^TYPE_CHECKED_EXPR_5^#.bar
// TYPE_CHECKED_EXPR_5: Begin completions
// TYPE_CHECKED_EXPR_5-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_5: End completions

