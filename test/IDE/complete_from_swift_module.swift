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

// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=AMBIGUOUS_RESULT_BUILER > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=AMBIGUOUS_RESULT_BUILER < %t.compl.txt

// ERROR_COMMON: found code completion token

import foo_swift_module
import corrupted_module

func testQualifyingModulesSuggested() -> #^QUALIFYING_MODULE^# {
  let x = #^QUALIFYING_MODULE_2^#
  // QUALIFYING_MODULE-DAG: Decl[Module]/None: swift_ide_test[#Module#]; name=swift_ide_test
  // QUALIFYING_MODULE-DAG: Decl[Module]/None/IsSystem: Swift[#Module#]; name=Swift
  // QUALIFYING_MODULE-DAG: Decl[Module]/None: foo_swift_module[#Module#]; name=foo_swift_module
}

struct SomeStructInThisModule {}
func testQualifyingModulesNotSuggested() {
  let x: swift_ide_test.#^ALREADY_QUALIFIED^#;
  // ALREADY_QUALIFIED-NOT: Decl[Module]
  // ALREADY_QUALIFIED-NOT: name=Type
  // ALREADY_QUALIFIED: name=SomeStructInThisModule
  // ALREADY_QUALIFIED-NOT: Decl[Module]
  // ALREADY_QUALIFIED-NOT: name=Type
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
// MODULE_QUALIFIED_2-DAG: Keyword[self]/CurrNominal: self[#FooSwiftStruct.Type#]; name=self
// MODULE_QUALIFIED_2-DAG: Keyword/CurrNominal: Type[#FooSwiftStruct.Type#]; name=Type
// MODULE_QUALIFIED_2-DAG: Decl[InstanceMethod]/CurrNominal: fooInstanceFunc({#(self): FooSwiftStruct#})[#() -> Void#]{{; name=.+$}}
// MODULE_QUALIFIED_2-DAG: Decl[Constructor]/CurrNominal: init()[#FooSwiftStruct#]{{; name=.+$}}

func testCompleteModuleQualified3() {
  foo_swift_module.BarGenericSwiftStruct1#^MODULE_QUALIFIED_3^#
}
// MODULE_QUALIFIED_3-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ({#t: _#})[#BarGenericSwiftStruct1<_>#]; name=(t:)
// MODULE_QUALIFIED_3-DAG: Decl[InstanceMethod]/CurrNominal: .bar1InstanceFunc({#(self): BarGenericSwiftStruct1<_>#})[#() -> Void#]; name=bar1InstanceFunc(:)

func testCompleteModuleQualified4() {
  foo_swift_module.BarGenericSwiftStruct2#^MODULE_QUALIFIED_4^#
}
// MODULE_QUALIFIED_4-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ({#t: _#}, {#u: _#})[#BarGenericSwiftStruct2<_, _>#]; name=(t:u:)
// MODULE_QUALIFIED_4-DAG: Decl[InstanceMethod]/CurrNominal: .bar2InstanceFunc({#(self): BarGenericSwiftStruct2<_, _>#})[#() -> Void#]; name=bar2InstanceFunc(:)
// MODULE_QUALIFIED_4-DAG: Keyword[self]/CurrNominal: .self[#BarGenericSwiftStruct2<_, _>.Type#]; name=self
// MODULE_QUALIFIED_4-DAG: Keyword/CurrNominal: .Type[#BarGenericSwiftStruct2<_, _>.Type#]; name=Type

func testCompleteModuleQualified5() {
  corrupted_module.#^MODULE_QUALIFIED_5^#
}

func testPostfixOperator1(x: Int) {
  x#^POSTFIX_OPERATOR_1^#
}

// POSTFIX_OPERATOR_1-DAG: Decl[PostfixOperatorFunction]/OtherModule[foo_swift_module]: =>[#Int#]
// POSTFIX_OPERATOR_1-DAG: Decl[InfixOperatorFunction]/OtherModule[foo_swift_module]:  %%% {#Int#}[#Int#]
// NEGATIVE_POSTFIX_OPERATOR_1-NOT: =->

#^TOP_LEVEL_1^#
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:  testCompleteModuleQualified1()[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/OtherModule[foo_swift_module]: visibleImport()[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[GlobalVar]/Local:     hiddenImport[#Int#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[GlobalVar]/OtherModule[foo_swift_module]:     globalVar[#Int#]{{; name=.+$}}

struct Foo: Swift.Array.#^STDLIB_TYPE_QUALIFIED_NESTED^# {}
// STDLIB_TYPE_QUALIFIED_NESTED: Decl[TypeAlias]/CurrNominal/IsSystem: Index[#Int#]; name=Index
// STDLIB_TYPE_QUALIFIED_NESTED: Decl[TypeAlias]/CurrNominal/IsSystem: Element[#Element#]; name=Element
// STDLIB_TYPE_QUALIFIED_NESTED: Keyword/None: Type[#Array.Type#]; name=Type

struct Bar: Swift.#^STDLIB_TYPE_QUALIFIED^# {}
// STDLIB_TYPE_QUALIFIED-NOT: Decl[Module]
// STDLIB_TYPE_QUALIFIED: Decl[Struct]/OtherModule[Swift]/IsSystem: AnyCollection[#AnyCollection<Element>#]; name=AnyCollection
// STDLIB_TYPE_QUALIFIED-NOT: Decl[Module]

func foo() -> foo_swift_module.#^MODULE_TYPE_QUALIFIED^# {}
// MODULE_TYPE_QUALIFIED: Decl[Protocol]/OtherModule[foo_swift_module]: BarProtocol[#BarProtocol#]; name=BarProtocol
// MODULE_TYPE_QUALIFIED: Decl[Enum]/OtherModule[foo_swift_module]: MyQuickLookObject[#MyQuickLookObject#]; name=MyQuickLookObject
// MODULE_TYPE_QUALIFIED: Decl[Struct]/OtherModule[foo_swift_module]: BarGenericSwiftStruct1[#BarGenericSwiftStruct1<T>#]; name=BarGenericSwiftStruct1
// MODULE_TYPE_QUALIFIED: Decl[Struct]/OtherModule[foo_swift_module]: FooSwiftStruct[#FooSwiftStruct#]; name=FooSwiftStruct
// MODULE_TYPE_QUALIFIED: Decl[Struct]/OtherModule[foo_swift_module]: BarGenericSwiftStruct2[#BarGenericSwiftStruct2<T, U>#]; name=BarGenericSwiftStruct2

// rdar://92048610
func testAmbiguousResultBuilder() {
  @resultBuilder
  struct MyBuilder {
    static func buildBlock(_ x: Int) -> Int {}
  }

  struct Foo {
    init(arg: Int = 1, @MyBuilder content: () -> Int) {}
    init(arg: Int = 1) {}
  }

  func test() {
    Foo {
      #^AMBIGUOUS_RESULT_BUILER^#
    }
    // Results should only contain globalVar once
    // AMBIGUOUS_RESULT_BUILER-NOT: globalVar
    // AMBIGUOUS_RESULT_BUILER-DAG: Decl[GlobalVar]/OtherModule[foo_swift_module]/TypeRelation[Convertible]: globalVar[#Int#]; name=globalVar
    // AMBIGUOUS_RESULT_BUILER-NOT: globalVar
  }
}
