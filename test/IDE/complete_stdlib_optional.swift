// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_NO_DOT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_NO_DOT_2 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DOT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DOT_1_SPACES > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT_SPACES < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DOT_2 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=UN_OPT_NO_DOT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=UN_OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=UN_OPT_DOT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=UN_OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=UN_OPT_DOT_2 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=UN_OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=UN_OPT_NO_DOT_2 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=UN_OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_TUPLE_1 | %FileCheck %s -check-prefix=OPT_TUPLE_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_TUPLE_2 | %FileCheck %s -check-prefix=OPT_TUPLE_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_TUPLE_3 | %FileCheck %s -check-prefix=OPT_TUPLE_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_TUPLE_4 | %FileCheck %s -check-prefix=OPT_TUPLE_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_TUPLE_5 | %FileCheck %s -check-prefix=OPT_TUPLE_5

//===---
//===--- Test code completion for stdlib Optional<T> type.
//===---

//===---
//===--- Helper types and functions that are used in this test.
//===---

struct FooStruct {
  var instanceVar: Int = 0
  func instanceFunc() {}
}

func returnsOptional() -> FooStruct? {
  return FooStruct()
}

func returnsImplicitlyUnwrappedOptional() -> FooStruct! {
  return FooStruct()
}

// OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal: ?.instanceFunc()[#Void#]{{; name=.+$}}

// OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal/Erase[1]:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal/Erase[1]: ?.instanceFunc()[#Void#]{{; name=.+$}}

// OPT_DOT_FOOSTRUCT_SPACES-DAG: Decl[InstanceVar]/CurrNominal/Erase[3]:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_DOT_FOOSTRUCT_SPACES-DAG: Decl[InstanceMethod]/CurrNominal/Erase[3]: ?.instanceFunc()[#Void#]{{; name=.+$}}

// UN_OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// UN_OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Void#]{{; name=.+$}}


//===---
//===--- Tests.
//===---

func testOptionalVar1(a: FooStruct?) {
  a#^OPT_NO_DOT_1^#
}

func testOptionalVar2(a: FooStruct?) {
  a.#^OPT_DOT_1^#
}

func testOptionalVar3(a: FooStruct?) {
  a.  #^OPT_DOT_1_SPACES^#
}

func testImplicitlyUnwrappedOptionalVar1(a: FooStruct!) {
  a#^UN_OPT_NO_DOT_1^#
}

func testImplicitlyUnwrappedOptionalVar2(a: FooStruct!) {
  a.#^UN_OPT_DOT_1^#
}

func testOptionalReturnValue1() {
  returnsOptional()#^OPT_NO_DOT_2^#
}

func testOptionalReturnValue2() {
  returnsOptional().#^OPT_DOT_2^#
}

func testImplicitlyUnwrappedOptionalReturnValue1() {
  returnsImplicitlyUnwrappedOptional()#^UN_OPT_NO_DOT_2^#
}

func testImplicitlyUnwrappedOptionalReturnValue2() {
  returnsImplicitlyUnwrappedOptional().#^UN_OPT_DOT_2^#
}

func testOptionalTuple1(a: (Int, String)?) {
  a#^OPT_TUPLE_1^#
}
// OPT_TUPLE_1: Pattern/CurrNominal:                ?.0[#Int#]
// OPT_TUPLE_1: Pattern/CurrNominal:                ?.1[#String#]

func testOptionalTuple2(a: (Int, String)?) {
  a.#^OPT_TUPLE_2^#
}
// OPT_TUPLE_2: Pattern/CurrNominal/Erase[1]:       ?.0[#Int#]
// OPT_TUPLE_2: Pattern/CurrNominal/Erase[1]:       ?.1[#String#]

func testOptionalTuple3(a: (Int, String)?) {
  a?#^OPT_TUPLE_3^#
}
// OPT_TUPLE_3: Pattern/CurrNominal:                .0[#Int#]
// OPT_TUPLE_3: Pattern/CurrNominal:                .1[#String#]

func testOptionalTuple4(a: (x: Int, y: String)?) {
  a#^OPT_TUPLE_4^#
}
// OPT_TUPLE_4: Pattern/CurrNominal:                ?.x[#Int#]
// OPT_TUPLE_4: Pattern/CurrNominal:                ?.y[#String#]

func testOptionalTuple5(a: (x: Int, y: String)?) {
  a.#^OPT_TUPLE_5^#
}
// OPT_TUPLE_5: Pattern/CurrNominal/Erase[1]:       ?.x[#Int#]
// OPT_TUPLE_5: Pattern/CurrNominal/Erase[1]:       ?.y[#String#]

// UN_OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal:    .instanceVar[#Int#]{{; name=.+$}}
// UN_OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal: .instanceFunc()[#Void#]{{; name=.+$}}

