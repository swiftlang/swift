// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_1 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_2 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_3 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_4 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_5 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_5

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_1 | FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_1 | FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_1_NEGATIVE
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_1 | FileCheck %s -check-prefix=NEGATIVE
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_2 | FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_2

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 | FileCheck %s -check-prefix=NEGATIVE
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_2 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_2 | FileCheck %s -check-prefix=NEGATIVE

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
// TYPE_CHECKED_EXPR_1-NEXT: SwiftDecl: .instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_1-NEXT: Keyword: .metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// TYPE_CHECKED_EXPR_1-NEXT: End completions

func resyncParser2() {}

// Test that we can code complete after a top-level var decl.
var _tmpVar1 : FooStruct

fooObject#^TYPE_CHECKED_EXPR_2^#
// TYPE_CHECKED_EXPR_2: Begin completions
// TYPE_CHECKED_EXPR_2-NEXT: SwiftDecl: .instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_2-NEXT: Keyword: .metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// TYPE_CHECKED_EXPR_2-NEXT: End completions

func resyncParser3() {}

fooObject#^TYPE_CHECKED_EXPR_3^#.bar
// TYPE_CHECKED_EXPR_3: Begin completions
// TYPE_CHECKED_EXPR_3-NEXT: SwiftDecl: .instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_3-NEXT: Keyword: .metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// TYPE_CHECKED_EXPR_3-NEXT: End completions

func resyncParser4() {}

fooObject.#^TYPE_CHECKED_EXPR_4^#
// TYPE_CHECKED_EXPR_4: Begin completions
// TYPE_CHECKED_EXPR_4-NEXT: SwiftDecl: instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_4-NEXT: Keyword: metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// TYPE_CHECKED_EXPR_4-NEXT: End completions

func resyncParser5() {}

fooObject.#^TYPE_CHECKED_EXPR_5^#.bar
// TYPE_CHECKED_EXPR_5: Begin completions
// TYPE_CHECKED_EXPR_5-NEXT: SwiftDecl: instanceVar[#Int#]{{$}}
// TYPE_CHECKED_EXPR_5-NEXT: Keyword: metatype[#[byref(implicit)] FooStruct.metatype#]{{$}}
// TYPE_CHECKED_EXPR_5-NEXT: End completions

func resyncParser5a() {}

fooObject.is#^TYPE_CHECKED_EXPR_KW_1^#
// TYPE_CHECKED_EXPR_KW_1: found code completion token
// TYPE_CHECKED_EXPR_KW_1-NOT: Begin completions

func resyncParser6() {}

var topLevelVar1 = #^TOP_LEVEL_VAR_INIT_1^#
// TOP_LEVEL_VAR_INIT_1: Begin completions
// TOP_LEVEL_VAR_INIT_1-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// TOP_LEVEL_VAR_INIT_1-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// TOP_LEVEL_VAR_INIT_1: End completions

// Check that the variable itself does not show up.
// TOP_LEVEL_VAR_INIT_1_NEGATIVE-NOT: SwiftDecl: topLevelVar1

func resyncParser7() {}

var topLevelVar2 = FooStruct#^TOP_LEVEL_VAR_INIT_2^#
// TOP_LEVEL_VAR_INIT_2: Begin completions
// TOP_LEVEL_VAR_INIT_2-NEXT: SwiftDecl: ({#instanceVar: Int#})[#FooStruct#]{{$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: SwiftDecl: ()[#FooStruct#]{{$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: Keyword: .metatype[#FooStruct.metatype.metatype#]{{$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: End completions

func resyncParser8() {}

#^PLAIN_TOP_LEVEL_1^#
// PLAIN_TOP_LEVEL_1: Begin completions
// PLAIN_TOP_LEVEL_1-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// PLAIN_TOP_LEVEL_1-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// PLAIN_TOP_LEVEL_1: End completions

func resyncParser9() {}

// Test that we can code complete immediately after a decl with a syntax error.
func _tmpFuncWithSyntaxError() { if return }

#^PLAIN_TOP_LEVEL_2^#
// PLAIN_TOP_LEVEL_2: Begin completions
// PLAIN_TOP_LEVEL_2-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// PLAIN_TOP_LEVEL_2-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// PLAIN_TOP_LEVEL_2: End completions

func resyncParser10() {}

//===--- Don't add any tests after this line.
// These declarations should not show up in top-level code completion results
// because forward references are not allowed at the top level.
extension FooStruct {
  func instanceFuncAtEOF() {}
// NEGATIVE-NOT: SwiftDecl: {{.*}}instanceFuncAtEOF
}

var varAtEOF : Int
// NEGATIVE-NOT: SwiftDecl: {{.*}}varAtEOF

