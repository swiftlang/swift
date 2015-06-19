// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_1 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_2 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_3 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_4 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_5 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_6 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_KW_1 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_KW_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1 | FileCheck %s -check-prefix=TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_1 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_1 < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_1_NEGATIVE < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_INIT_2 | FileCheck %s -check-prefix=TOP_LEVEL_VAR_INIT_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL_NO_DUPLICATES < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_2 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_2 | FileCheck %s -check-prefix=NEGATIVE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE_1 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_VAR_TYPE_1 < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_1 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_2 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_3 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_4 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_5 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_STMT_5 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_6 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_STMT_6 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_7 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_STMT_7 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_8 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_STMT_8 < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_9 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_STMT_10 | FileCheck %s -check-prefix=PLAIN_TOP_LEVEL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_AUTOCLOSURE_1 | FileCheck %s -check-prefix=AUTOCLOSURE_STRING


// Test code completion in top-level code.
//
// This test is not meant to test that we can correctly form all kinds of
// completion results in general; that should be tested elsewhere.

struct FooStruct {
  var instanceVar = 0

  func instanceFunc(a: Int) {}
  // Add more stuff as needed.
}

var fooObject : FooStruct

func fooFunc1() {}
func fooFunc2(a: Int, _ b: Double) {}

func erroneous1(x: Undeclared) {}

//===--- Test code completions of expressions that can be typechecked.

// Although the parser can recover in most of these test cases, we resync it
// anyway to ensure that there parser recovery does not interfere with code
// completion.
func resyncParser1() {}

fooObject#^TYPE_CHECKED_EXPR_1^#
// TYPE_CHECKED_EXPR_1: Begin completions
// TYPE_CHECKED_EXPR_1-NEXT: Decl[InstanceVar]/CurrNominal: .instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_1-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_1-NEXT: End completions

func resyncParser2() {}

// Test that we can code complete after a top-level var decl.
var _tmpVar1 : FooStruct

fooObject#^TYPE_CHECKED_EXPR_2^#
// TYPE_CHECKED_EXPR_2: Begin completions
// TYPE_CHECKED_EXPR_2-NEXT: Decl[InstanceVar]/CurrNominal: .instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_2-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_2-NEXT: End completions

func resyncParser3() {}

fooObject#^TYPE_CHECKED_EXPR_3^#.bar
// TYPE_CHECKED_EXPR_3: Begin completions
// TYPE_CHECKED_EXPR_3-NEXT: Decl[InstanceVar]/CurrNominal: .instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_3-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_3-NEXT: End completions

func resyncParser4() {}

fooObject.#^TYPE_CHECKED_EXPR_4^#
// TYPE_CHECKED_EXPR_4: Begin completions
// TYPE_CHECKED_EXPR_4-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_4-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_4-NEXT: End completions

func resyncParser5() {}

fooObject.#^TYPE_CHECKED_EXPR_5^#.bar
// TYPE_CHECKED_EXPR_5: Begin completions
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
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Void#]{{; name=.+$}}
// TYPE_CHECKED_EXPR_WITH_ERROR_IN_INIT_1-NEXT: End completions

func resyncParser6a() {}

var topLevelVar1 = #^TOP_LEVEL_VAR_INIT_1^#
// TOP_LEVEL_VAR_INIT_1: Begin completions
// TOP_LEVEL_VAR_INIT_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_1-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_1: End completions

// Check that the variable itself does not show up.
// TOP_LEVEL_VAR_INIT_1_NEGATIVE-NOT: topLevelVar1

func resyncParser7() {}

var topLevelVar2 = FooStruct#^TOP_LEVEL_VAR_INIT_2^#
// TOP_LEVEL_VAR_INIT_2: Begin completions
// TOP_LEVEL_VAR_INIT_2-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc({#self: FooStruct#})[#(Int) -> Void#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: Decl[Constructor]/CurrNominal: ({#instanceVar: Int#})[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: Decl[Constructor]/CurrNominal: ()[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_INIT_2-NEXT: End completions

func resyncParser8() {}

#^PLAIN_TOP_LEVEL_1^#
// PLAIN_TOP_LEVEL: Begin completions
// PLAIN_TOP_LEVEL-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// PLAIN_TOP_LEVEL-DAG: Decl[GlobalVar]/CurrModule: fooObject[#FooStruct#]{{; name=.+$}}
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

//===--- Test code completions of types.

func resyncParserA1() {}

var topLevelVarType1 : #^TOP_LEVEL_VAR_TYPE_1^#
// TOP_LEVEL_VAR_TYPE_1: Begin completions
// TOP_LEVEL_VAR_TYPE_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// TOP_LEVEL_VAR_TYPE_1: End completions

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
// TOP_LEVEL_STMT_5: Decl[LocalVar]/Local: i[#Int#]{{; name=.+$}}
// TOP_LEVEL_STMT_5: End completions
}

func resyncParserB6() {}

for i in [] {
  #^TOP_LEVEL_STMT_6^#
// TOP_LEVEL_STMT_6: Begin completions
// TOP_LEVEL_STMT_6: Decl[LocalVar]/Local: i[#<<error type>>#]{{; name=.+$}}
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

//===--- Don't add any tests after this line.
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

// rdar://20738314
if true {
    var z = #^TOP_LEVEL_STMT_10^#
} else {
    assertionFailure("Shouldn't be here")
}

// rdar://21346928
func optStr() -> String? { return nil }
let x = (optStr() ?? "autoclosure").#^TOP_LEVEL_AUTOCLOSURE_1^#
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      characters[#String.CharacterView#]
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      utf16[#String.UTF16View#]
// AUTOCLOSURE_STRING: Decl[InstanceVar]/CurrNominal:      utf8[#String.UTF8View#]
