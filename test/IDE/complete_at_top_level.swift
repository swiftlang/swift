// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_1 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_2 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_3 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_4 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_5 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_6 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_KW_1 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_KW_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1 | %FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_1 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_1 < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_1_NEGATIVE < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_2 | %FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL_NO_DUPLICATES < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_2 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_2 | %FileCheck %s -check-prefix=NEGATIVE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_CLOSURE_1 | %FileCheck %s -check-prefix=TOP_LEVEL_CLOSURE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_1 > %t.toplevel.1.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.1.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.1.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.1.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_2 > %t.toplevel.2.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.2.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.2.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.2.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_3 > %t.toplevel.3.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.3.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.3.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.3.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_4 > %t.toplevel.4.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.4.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.4.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.4.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_5 > %t.toplevel.5.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.5.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.5.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.5.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_5 > %t.toplevel.5.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.5.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.5.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.5.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_6 > %t.toplevel.6.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.6.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel.6.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.6.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_EXPR_TYPE_1 > %t.toplevel-expr.1.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel-expr.1.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel-expr.1.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel-expr.1.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_EXPR_TYPE_2 > %t.toplevel-expr.2.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel-expr.2.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel-expr.2.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel-expr.2.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_EXPR_TYPE_3 > %t.toplevel-expr.3.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel-expr.3.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel-expr.3.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel-expr.3.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_EXPR_TYPE_4 > %t.toplevel-expr.4.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel-expr.4.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel-expr.4.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel-expr.4.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_EXPR_TYPE_2 > %t.toplevel-expr.2.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel-expr.2.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_NEGATIVE_1 < %t.toplevel-expr.2.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.toplevel-expr.2.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_1 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_2 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_3 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_4 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_5 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_STMT_5 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_6 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_STMT_6 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_7 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_STMT_7 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_8 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_STMT_8 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_9 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_10 | %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_AUTOCLOSURE_1 | %FileCheck %s -check-prefix=AUTOCLOSURE_STRING

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_SWITCH_CASE_1 | %FileCheck %s -check-prefix=TOP_LEVEL_SWITCH_CASE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_BEFORE_GUARD_NAME_1 | %FileCheck %s -check-prefix=TOP_LEVEL_BEFORE_GUARD_NAME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_BEFORE_GUARD_NAME_2 | %FileCheck %s -check-prefix=TOP_LEVEL_BEFORE_GUARD_NAME

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_GUARD_1 | %FileCheck %s -check-prefix=TOP_LEVEL_GUARD
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_GUARD_2 | %FileCheck %s -check-prefix=TOP_LEVEL_GUARD

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRING_INTERP_1 | %FileCheck %s -check-prefix=STRING_INTERP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRING_INTERP_2 | %FileCheck %s -check-prefix=STRING_INTERP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRING_INTERP_3 | %FileCheck %s -check-prefix=STRING_INTERP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRING_INTERP_4 | %FileCheck %s -check-prefix=STRING_INTERP

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_1 > %t.for_collection1
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection1
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_2 > %t.for_collection2
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection2
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_3 > %t.for_collection3
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection3
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_4 > %t.for_collection4
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection4
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_5 > %t.for_collection5
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection5
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_6 > %t.for_collection6
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection6
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_7 > %t.for_collection7
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection7
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection7
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_COLLECTION_8 > %t.for_collection8
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.for_collection8
// RUN: %FileCheck %s -check-prefix=FOR_COLLECTION < %t.for_collection8

// Test code completion in top-level code.
//
// This test is not meant to test that we can correctly form all kinds of
// completion results in general; that should be tested elsewhere.

struct FooStruct {
  var instanceVar = 0

  func instanceFunc(_ a: Int) {}
  // Add more stuff as needed.
}

var fooObject : FooStruct

func fooFunc1() {}
func fooFunc2(_ a: Int, _ b: Double) {}

func erroneous1(_ x: Undeclared) {}

//===--- Test code completions of expressions that can be typechecked.

// Although the parser can recover in most of these test cases, we resync it
// anyway to ensure that there parser recovery does not interfere with code
// completion.
func resyncParser1() {}

fooObject#^TYPE_CHECKED_EXPR_1^#
// TYPE_CHECKED_EXPR_1: Begin completions
// TYPE_CHECKED_EXPR_1-NEXT: Decl[InstanceVar]/CurrNominal:      .instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_1-NEXT: BuiltinOperator/None:               = {#FooStruct#}[#Void#]; name== FooStruct
// TYPE_CHECKED_EXPR_1-NEXT: Keyword[self]/CurrNominal:          .self[#FooStruct#]; name=self
// TYPE_CHECKED_EXPR_1-NEXT: End completions

func resyncParser2() {}

// Test that we can code complete after a top-level var decl.
var _tmpVar1 : FooStruct

fooObject#^TYPE_CHECKED_EXPR_2^#
// TYPE_CHECKED_EXPR_2: Begin completions
// TYPE_CHECKED_EXPR_2-NEXT: Decl[InstanceVar]/CurrNominal: .instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_2-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_2-NEXT: BuiltinOperator/None:                     = {#FooStruct#}[#Void#]; name== FooStruct
// TYPE_CHECKED_EXPR_2-NEXT: Keyword[self]/CurrNominal: .self[#FooStruct#]; name=self
// TYPE_CHECKED_EXPR_2-NEXT: End completions

func resyncParser3() {}

fooObject#^TYPE_CHECKED_EXPR_3^#.bar
// TYPE_CHECKED_EXPR_3: Begin completions
// TYPE_CHECKED_EXPR_3-NEXT: Decl[InstanceVar]/CurrNominal: .instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_3-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_3-NEXT: BuiltinOperator/None:                     = {#FooStruct#}[#Void#]; name== FooStruct
// TYPE_CHECKED_EXPR_3-NEXT: Keyword[self]/CurrNominal: .self[#FooStruct#]; name=self
// TYPE_CHECKED_EXPR_3-NEXT: End completions

func resyncParser4() {}

fooObject.#^TYPE_CHECKED_EXPR_4^#
// TYPE_CHECKED_EXPR_4: Begin completions
// TYPE_CHECKED_EXPR_4-NEXT: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// TYPE_CHECKED_EXPR_4-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_4-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_4-NEXT: End completions

func resyncParser5() {}

fooObject.#^TYPE_CHECKED_EXPR_5^#.bar
// TYPE_CHECKED_EXPR_5: Begin completions
// TYPE_CHECKED_EXPR_5-NEXT: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// TYPE_CHECKED_EXPR_5-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_5-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_5-NEXT: End completions

func resyncParser6() {}

fooObject.instanceFunc(#^TYPE_CHECKED_EXPR_6^#

func resyncParser6() {}

fooObject.is#^TYPE_CHECKED_EXPR_KW_1^#
// TYPE_CHECKED_EXPR_KW_1: found code completion token
// TYPE_CHECKED_EXPR_KW_1-NOT: Begin completions

func resyncParser7() {}

// We have an error in the initializer here, but the type is explicitly written
// in the source.
var fooObjectWithErrorInInit : FooStruct = unknown_var

fooObjectWithErrorInInit.#^TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1^#
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1: Begin completions
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: End completions

func resyncParser6a() {}

var topLevelVar1 = #^TOP_LEVEL_VAR_INIT_1^#
// TOP_LEVEL_VAR_INIT_1: Begin completions
// TOP_LEVEL_VAR_INIT_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_1-DAG: Decl[FreeFunction]/CurrModule: fooFunc1()[#Void#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_1-DAG: Decl[GlobalVar]/Local: fooObject[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_1: End completions

// Check that the variable itself does not show up.
// TOP_LEVEL_VAR_INIT_1_NEGATIVE-NOT: topLevelVar1

func resyncParser7() {}

var topLevelVar2 = FooStruct#^TOP_LEVEL_VAR_INIT_2^#
// TOP_LEVEL_VAR_INIT_2: Begin completions
// TOP_LEVEL_VAR_INIT_2-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#self: FooStruct#})[#(Int) -> Void#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: Decl[Constructor]/CurrNominal: ({#instanceVar: Int#})[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: Decl[Constructor]/CurrNominal: ()[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: Keyword[self]/CurrNominal: .self[#FooStruct.Type#]; name=self
// TOP_LEVEL_VAR_INIT_2-NEXT: End completions

func resyncParser8() {}

#^PLAIN_TOP_LEVEL_1^#
// PLAIN_TOP_LEVEL: Begin completions
// PLAIN_TOP_LEVEL-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// PLAIN_TOP_LEVEL-DAG: Decl[GlobalVar]/Local: fooObject[#FooStruct#]{{; name=.+$}}
// PLAIN_TOP_LEVEL: End completions

// PLAIN_TOP_LEVEL_NO_DUPLICATES: Begin completions
// PLAIN_TOP_LEVEL_NO_DUPLICATES-DAG: Decl[FreeFunction]/CurrModule: fooFunc1()[#Void#]{{; name=.+$}}
// PLAIN_TOP_LEVEL_NO_DUPLICATES-DAG: Decl[FreeFunction]/CurrModule: fooFunc2({#(a): Int#}, {#(b): Double#})[#Void#]{{; name=.+$}}
// PLAIN_TOP_LEVEL_NO_DUPLICATES-NOT: fooFunc1
// PLAIN_TOP_LEVEL_NO_DUPLICATES-NOT: fooFunc2
// PLAIN_TOP_LEVEL_NO_DUPLICATES: End completions

func resyncParser9() {}

// Test that we can code complete immediately after a decl with a syntax error.
func _tmpFuncWithSyntaxError() { if return }

#^PLAIN_TOP_LEVEL_2^#

func resyncParser10() {}

_ = {
  #^TOP_LEVEL_CLOSURE_1^#
}()
// TOP_LEVEL_CLOSURE_1: Begin completions
// TOP_LEVEL_CLOSURE_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_CLOSURE_1-DAG: Decl[FreeFunction]/CurrModule: fooFunc1()[#Void#]{{; name=.+$}}
// TOP_LEVEL_CLOSURE_1-DAG: Decl[GlobalVar]/Local: fooObject[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_CLOSURE_1: End completions

func resyncParser11() {}

//===--- Test code completions of types.

func resyncParserA1() {}

var topLevelVarType1 : #^TOP_LEVEL_VAR_TYPE_1^#
// TOP_LEVEL_VAR_TYPE_1: Begin completions
// TOP_LEVEL_VAR_TYPE_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_TYPE_1: End completions
// TOP_LEVEL_VAR_TYPE_NEGATIVE_1-NOT: Decl[GlobalVar
// TOP_LEVEL_VAR_TYPE_NEGATIVE_1-NOT: Decl[FreeFunc
func resyncParserA1_1() {}

var topLevelVarType2 : [#^TOP_LEVEL_VAR_TYPE_2^#]

func resyncParserA1_2() {}

var topLevelVarType3 : [#^TOP_LEVEL_VAR_TYPE_3^#: Int]

func resyncParserA1_3() {}

var topLevelVarType4 : [Int: #^TOP_LEVEL_VAR_TYPE_4^#]

func resyncParserA1_4() {}

if let topLevelVarType5 : [#^TOP_LEVEL_VAR_TYPE_5^#] {}

func resyncParserA1_5() {}

guard let topLevelVarType6 : [#^TOP_LEVEL_VAR_TYPE_6^#] else {}

func resyncParserA1_6() {}

_ = ("a" as #^TOP_LEVEL_EXPR_TYPE_1^#)
func resyncParserA1_7() {}
_ = ("a" as! #^TOP_LEVEL_EXPR_TYPE_2^#)
func resyncParserA1_8() {}
_ = ("a" as? #^TOP_LEVEL_EXPR_TYPE_3^#)
func resyncParserA1_9() {}
_ = ("a" is #^TOP_LEVEL_EXPR_TYPE_4^#)

func resyncParserA2() {}

//===--- Test code completion in statements.

func resyncParserB1() {}

if (true) {
  #^TOP_LEVEL_STMT_1^#
}

func resyncParserB2() {}

while (true) {
  #^TOP_LEVEL_STMT_2^#
}

func resyncParserB3() {}

repeat {
  #^TOP_LEVEL_STMT_3^#
} while true

func resyncParserB4() {}

for ; ; {
  #^TOP_LEVEL_STMT_4^#
}

func resyncParserB5() {}

for var i = 0; ; {
  #^TOP_LEVEL_STMT_5^#
// TOP_LEVEL_STMT_5: Begin completions
// TOP_LEVEL_STMT_5: Decl[LocalVar]/Local: i[#<<error type>>#]{{; name=.+$}}
// TOP_LEVEL_STMT_5: End completions
}

func resyncParserB6() {}

for i in [] {
  #^TOP_LEVEL_STMT_6^#
// TOP_LEVEL_STMT_6: Begin completions
// TOP_LEVEL_STMT_6: Decl[LocalVar]/Local: i[#Any#]{{; name=.+$}}
// TOP_LEVEL_STMT_6: End completions
}

func resyncParserB7() {}

for i in [1, 2, 3] {
  #^TOP_LEVEL_STMT_7^#
// TOP_LEVEL_STMT_7: Begin completions
// TOP_LEVEL_STMT_7: Decl[LocalVar]/Local: i[#Int#]{{; name=.+$}}
// TOP_LEVEL_STMT_7: End completions
}

func resyncParserB8() {}

for i in unknown_var {
  #^TOP_LEVEL_STMT_8^#
// TOP_LEVEL_STMT_8: Begin completions
// TOP_LEVEL_STMT_8: Decl[LocalVar]/Local: i[#<<error type>>#]{{; name=.+$}}
// TOP_LEVEL_STMT_8: End completions
}

func resyncParserB9() {}

switch (0, 42) {
  case (0, 0):
    #^TOP_LEVEL_STMT_9^#
}

func resyncParserB10() {}

// rdar://20738314
if true {
    var z = #^TOP_LEVEL_STMT_10^#
} else {
    assertionFailure("Shouldn't be here")
}

func resyncParserB11() {}

// rdar://21346928
func optStr() -> String? { return nil }
let x = (optStr() ?? "autoclosure").#^TOP_LEVEL_AUTOCLOSURE_1^#
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      unicodeScalars[#String.UnicodeScalarView#]
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      utf16[#String.UTF16View#]

func resyncParserB12() {}

// rdar://21661308
switch 1 {
  case #^TOP_LEVEL_SWITCH_CASE_1^#
}
// TOP_LEVEL_SWITCH_CASE_1: Begin completions

func resyncParserB13() {}

#^TOP_LEVEL_BEFORE_GUARD_NAME_1^#
// TOP_LEVEL_BEFORE_GUARD_NAME-NOT: name=guardedName

guard let guardedName = 1 as Int? {
  #^TOP_LEVEL_BEFORE_GUARD_NAME_2^#
}

#^TOP_LEVEL_GUARD_1^#

func interstitial() {}

#^TOP_LEVEL_GUARD_2^#
// TOP_LEVEL_GUARD: Decl[LocalVar]/Local: guardedName[#Int#]; name=guardedName

func resyncParserB14() {}


"\(#^STRING_INTERP_1^#)"
"\(1) \(#^STRING_INTERP_2^#) \(2)"
var stringInterp = "\(#^STRING_INTERP_3^#)"
_ = "" + "\(#^STRING_INTERP_4^#)" + ""
// STRING_INTERP: Begin completions
// STRING_INTERP-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#];
// STRING_INTERP-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: fooFunc1()[#Void#];
// STRING_INTERP-DAG: Decl[FreeFunction]/CurrModule: optStr()[#String?#];
// STRING_INTERP-DAG: Decl[GlobalVar]/Local: fooObject[#FooStruct#];
// STRING_INTERP: End completions
func resyncParserC1() {}

// FOR_COLLECTION-NOT: forIndex
for forIndex in [#^FOR_COLLECTION_1^#] {}
for forIndex in [1,#^FOR_COLLECTION_2^#] {}
for forIndex in [1:#^FOR_COLLECTION_3^#] {}
for forIndex in [#^FOR_COLLECTION_4^#:] {}
for forIndex in [#^FOR_COLLECTION_5^#:2] {}
for forIndex in [1:2, #^FOR_COLLECTION_6^#] {}
for forIndex in [1:2, #^FOR_COLLECTION_7^#:] {}
for forIndex in [1:2, #^FOR_COLLECTION_8^#:2] {}


//
//===--- DON'T ADD ANY TESTS AFTER THIS LINE.
//
// These declarations should not show up in top-level code completion results
// because forward references are not allowed at the top level.

struct StructAtEOF {}
// NEGATIVE-NOT: StructAtEOF

extension FooStruct {
  func instanceFuncAtEOF() {}
// NEGATIVE-NOT: instanceFuncAtEOF
}

var varAtEOF : Int
// NEGATIVE-NOT: varAtEOF
