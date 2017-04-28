// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/foo_swift_module.swift
//
// Note: this test checks both module import case and file import case.

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
// MODULE_QUALIFIED_2-NEXT: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc({#self: FooSwiftStruct#})[#() -> Void#]{{; name=.+$}}
// MODULE_QUALIFIED_2-NEXT: Decl[Constructor]/CurrNominal: init()[#FooSwiftStruct#]{{; name=.+$}}
// MODULE_QUALIFIED_2-NEXT: End completions

func testCompleteModuleQualified3() {
  foo_swift_module.BarGenericSwiftStruct1#^MODULE_QUALIFIED_3^#
}
// MODULE_QUALIFIED_3: Begin completions
// MODULE_QUALIFIED_3-NEXT: Decl[Constructor]/CurrNominal:    ({#t: τ_0_0#})[#BarGenericSwiftStruct1<τ_0_0>#]
// MODULE_QUALIFIED_3-NEXT: Decl[InstanceMethod]/CurrNominal: .bar1InstanceFunc({#self: BarGenericSwiftStruct1<τ_0_0>#})[#() -> Void#]{{; name=.+$}}
// MODULE_QUALIFIED_3: Decl[InfixOperatorFunction]/OtherModule[Swift]: != {#Any.Type?#}[#Bool#];
// MODULE_QUALIFIED_3: End completions

func testCompleteModuleQualified4() {
  foo_swift_module.BarGenericSwiftStruct2#^MODULE_QUALIFIED_4^#
}
// MODULE_QUALIFIED_4: Begin completions
// MODULE_QUALIFIED_4-NEXT: Decl[Constructor]/CurrNominal:    ({#t: BarProtocol#}, {#u: τ_0_1#})[#BarGenericSwiftStruct2<BarProtocol, τ_0_1>#]
// MODULE_QUALIFIED_4-NEXT: Decl[InstanceMethod]/CurrNominal: .bar2InstanceFunc({#self: BarGenericSwiftStruct2<BarProtocol, τ_0_1>#})[#() -> Void#]
// MODULE_QUALIFIED_4: Decl[InfixOperatorFunction]/OtherModule[Swift]: != {#Any.Type?#}[#Bool#];
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
