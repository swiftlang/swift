// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/foo_swift_module.swift
//
// Note: this test checks both module import case and file import case.

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=QUALIFYING_MODULE > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=QUALIFYING_MODULE < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=QUALIFYING_MODULE_2 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=QUALIFYING_MODULE < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=ALREADY_QUALIFIED > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=ALREADY_QUALIFIED < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=MODULE_QUALIFIED_1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=MODULE_QUALIFIED_1 < %t.compl.txt
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %S/Inputs -enable-source-import -code-completion-token=MODULE_QUALIFIED_1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=MODULE_QUALIFIED_1 < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=MODULE_QUALIFIED_2 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=MODULE_QUALIFIED_2 < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=MODULE_QUALIFIED_3 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=MODULE_QUALIFIED_3 < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=MODULE_QUALIFIED_4 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=MODULE_QUALIFIED_4 < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=MODULE_QUALIFIED_5 | %FileCheck %s -check-prefix=ERROR_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=STDLIB_TYPE_QUALIFIED_NESTED > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=STDLIB_TYPE_QUALIFIED_NESTED < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=STDLIB_TYPE_QUALIFIED > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=STDLIB_TYPE_QUALIFIED < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=MODULE_TYPE_QUALIFIED > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=MODULE_TYPE_QUALIFIED < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=POSTFIX_OPERATOR_1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=POSTFIX_OPERATOR_1 < %t.compl.txt
// RUN: %FileCheck %s -check-prefix=NEGATIVE_POSTFIX_OPERATOR_1 < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=TOP_LEVEL_1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_1 < %t.compl.txt
// rdar://15305873 Code completion: implement proper shadowing of declarations represented by cached results
// FIXME: %FileCheck %s -check-prefix=TOP_LEVEL_1_NEGATIVE < %t.compl.txt
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %S/Inputs -enable-source-import -code-completion-token=TOP_LEVEL_1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_1 < %t.compl.txt
// rdar://15305873 Code completion: implement proper shadowing of declarations represented by cached results
// FIXME: %FileCheck %s -check-prefix=TOP_LEVEL_1_NEGATIVE < %t.compl.txt

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

import foo_swift_module
import corrupted_module

func testQualifyingModulesSuggested() -> #^QUALIFYING_MODULE^# {
  let x = #^QUALIFYING_MODULE_2^#
  // QUALIFYING_MODULE: Begin completions
  // QUALIFYING_MODULE-DAG: Decl[Module]/None: swift_ide_test[#Module#]; name=swift_ide_test
  // QUALIFYING_MODULE-DAG: Decl[Module]/None: Swift[#Module#]; name=Swift
  // QUALIFYING_MODULE-DAG: Decl[Module]/None: foo_swift_module[#Module#]; name=foo_swift_module
  // QUALIFYING_MODULE: End completions
}

struct SomeStructInThisModule {}
func testQualifyingModulesNotSuggested() {
  let x: swift_ide_test.#^ALREADY_QUALIFIED^#;
  // ALREADY_QUALIFIED: Begin completions
  // ALREADY_QUALIFIED-NOT: Decl[Module]
  // ALREADY_QUALIFIED-NOT: name=Type
  // ALREADY_QUALIFIED: name=SomeStructInThisModule
  // ALREADY_QUALIFIED-NOT: Decl[Module]
  // ALREADY_QUALIFIED-NOT: name=Type
  // ALREADY_QUALIFIED: End completions
}

var hiddenImport : Int
// TOP_LEVEL_1_NEGATIVE-NOT: hiddenImport()

func testCompleteModuleQualified1() {
  foo_swift_module.#^MODULE_QUALIFIED_1^#
// Check that we don't include references to operators.
// MODULE_QUALIFIED_1-NOT: %%%
}

func testCompleteModuleQualified2() {
  foo_swift_module.FooSwiftStruct.#^MODULE_QUALIFIED_2^#
}
// MODULE_QUALIFIED_2: Begin completions
// MODULE_QUALIFIED_2-NEXT: Keyword[self]/CurrNominal: self[#FooSwiftStruct.Type#]; name=self
// MODULE_QUALIFIED_2-NEXT: Keyword/CurrNominal: Type[#FooSwiftStruct.Type#]; name=Type
// MODULE_QUALIFIED_2-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc({#(self): FooSwiftStruct#})[#() -> Void#]{{; name=.+$}}
// MODULE_QUALIFIED_2-NEXT: Decl[Constructor]/CurrNominal: init()[#FooSwiftStruct#]{{; name=.+$}}
// MODULE_QUALIFIED_2-NEXT: End completions

func testCompleteModuleQualified3() {
  foo_swift_module.BarGenericSwiftStruct1#^MODULE_QUALIFIED_3^#
}
// MODULE_QUALIFIED_3: Begin completions
// MODULE_QUALIFIED_3-NEXT: Decl[Constructor]/CurrNominal: ({#t: _#})[#BarGenericSwiftStruct1<_>#]; name=(t: _)
// MODULE_QUALIFIED_3-NEXT: Decl[InstanceMethod]/CurrNominal: .bar1InstanceFunc({#(self): BarGenericSwiftStruct1<_>#})[#() -> Void#]; name=bar1InstanceFunc(self: BarGenericSwiftStruct1<_>)
// MODULE_QUALIFIED_3: End completions

func testCompleteModuleQualified4() {
  foo_swift_module.BarGenericSwiftStruct2#^MODULE_QUALIFIED_4^#
}
// MODULE_QUALIFIED_4: Begin completions
// MODULE_QUALIFIED_4-NEXT: Decl[Constructor]/CurrNominal: ({#t: _#}, {#u: _#})[#BarGenericSwiftStruct2<_, _>#]; name=(t: _, u: _)
// MODULE_QUALIFIED_4-NEXT: Decl[InstanceMethod]/CurrNominal: .bar2InstanceFunc({#(self): BarGenericSwiftStruct2<_, _>#})[#() -> Void#]; name=bar2InstanceFunc(self: BarGenericSwiftStruct2<_, _>)
// MODULE_QUALIFIED_4-NEXT: Keyword[self]/CurrNominal: .self[#BarGenericSwiftStruct2<_, _>.Type#]; name=self
// MODULE_QUALIFIED_4-NEXT: Keyword/CurrNominal: .Type[#BarGenericSwiftStruct2<_, _>.Type#]; name=Type
// MODULE_QUALIFIED_4-NEXT: End completions

func testCompleteModuleQualified5() {
  corrupted_module.#^MODULE_QUALIFIED_5^#
}

func testPostfixOperator1(x: Int) {
  x#^POSTFIX_OPERATOR_1^#
}

// POSTFIX_OPERATOR_1: Begin completions
// POSTFIX_OPERATOR_1-DAG: Decl[PostfixOperatorFunction]/OtherModule[foo_swift_module]: =>[#Int#]
// POSTFIX_OPERATOR_1-DAG: Decl[InfixOperatorFunction]/OtherModule[foo_swift_module]:  %%% {#Int#}[#Int#]
// POSTFIX_OPERATOR_1: End completions
// NEGATIVE_POSTFIX_OPERATOR_1-NOT: =->

#^TOP_LEVEL_1^#
// TOP_LEVEL_1: Begin completions
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:  testCompleteModuleQualified1()[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/OtherModule[foo_swift_module]: visibleImport()[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[GlobalVar]/Local:     hiddenImport[#Int#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[GlobalVar]/OtherModule[foo_swift_module]:     globalVar[#Int#]{{; name=.+$}}
// TOP_LEVEL_1: End completions

struct Foo: Swift.Array.#^STDLIB_TYPE_QUALIFIED_NESTED^# {}
// STDLIB_TYPE_QUALIFIED_NESTED: Begin completions
// STDLIB_TYPE_QUALIFIED_NESTED: Decl[TypeAlias]/CurrNominal: Index[#Int#]; name=Index
// STDLIB_TYPE_QUALIFIED_NESTED: Decl[TypeAlias]/CurrNominal: Element[#Element#]; name=Element
// STDLIB_TYPE_QUALIFIED_NESTED: Keyword/None: Type[#Array.Type#]; name=Type
// STDLIB_TYPE_QUALIFIED_NESTED: End completions

struct Bar: Swift.#^STDLIB_TYPE_QUALIFIED^# {}
// STDLIB_TYPE_QUALIFIED: Begin completions
// STDLIB_TYPE_QUALIFIED-NOT: Decl[Module]
// STDLIB_TYPE_QUALIFIED: Decl[Struct]/OtherModule[Swift]:    AnyCollection[#AnyCollection#]; name=AnyCollection
// STDLIB_TYPE_QUALIFIED-NOT: Decl[Module]
// STDLIB_TYPE_QUALIFIED: End completions

func foo() -> foo_swift_module.#^MODULE_TYPE_QUALIFIED^# {}
// MODULE_TYPE_QUALIFIED: Begin completions
// MODULE_TYPE_QUALIFIED: Decl[Protocol]/OtherModule[foo_swift_module]: BarProtocol[#BarProtocol#]; name=BarProtocol
// MODULE_TYPE_QUALIFIED: Decl[Enum]/OtherModule[foo_swift_module]: MyQuickLookObject[#MyQuickLookObject#]; name=MyQuickLookObject
// MODULE_TYPE_QUALIFIED: Decl[Struct]/OtherModule[foo_swift_module]: BarGenericSwiftStruct1[#BarGenericSwiftStruct1#]; name=BarGenericSwiftStruct1
// MODULE_TYPE_QUALIFIED: Decl[Struct]/OtherModule[foo_swift_module]: FooSwiftStruct[#FooSwiftStruct#]; name=FooSwiftStruct
// MODULE_TYPE_QUALIFIED: Decl[Struct]/OtherModule[foo_swift_module]: BarGenericSwiftStruct2[#BarGenericSwiftStruct2#]; name=BarGenericSwiftStruct2
// MODULE_TYPE_QUALIFIED: End completions
