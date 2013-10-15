// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_NO_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_NO_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_NO_DOT_3 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_INT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DOT_3 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_DOT_INT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_NO_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBCCLASS_MEMBERS_NO_DOT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_NO_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBCCLASS_MEMBERS_NO_DOT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBCCLASS_MEMBERS_DOT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBCCLASS_MEMBERS_DOT < %t.opt.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_FORCE_RETURN_OPTIONAL_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_OBJCCLASS < %t.opt.txt

//===---
//===--- Test code completion for stdlib Optional<T> type.
//===---

//===---
//===--- Helper types and functions that are used in this test.
//===---

struct FooStruct {
  var instanceVar: Int
  func instanceFunc() {}
}

func returnsOptional() -> Optional<FooStruct> {
  return FooStruct()
}

@objc
class ObjcClass {
  var instanceVar: Int
  func instanceFunc() -> ObjcClass {}
}

// OPT_NO_DOT_FOOSTRUCT: Begin completions, 4 items
// OPT_NO_DOT_FOOSTRUCT-NEXT: Pattern/ExprSpecific: ![#FooStruct#]{{$}}
// OPT_NO_DOT_FOOSTRUCT-NEXT: Pattern/ExprSpecific: ?[#FooStruct#]{{$}}
// OPT_NO_DOT_FOOSTRUCT-NEXT: Decl/CurrNominal:     .getLogicValue()[#Bool#]{{$}}
// OPT_NO_DOT_FOOSTRUCT-NEXT: Keyword/None:         .metatype[#Optional<FooStruct>.metatype#]{{$}}
// OPT_NO_DOT_FOOSTRUCT-NEXT: End completions

// OPT_DOT_FOOSTRUCT: Begin completions, 2 items
// OPT_DOT_FOOSTRUCT-NEXT: Decl/CurrNominal: getLogicValue()[#Bool#]{{$}}
// OPT_DOT_FOOSTRUCT-NEXT: Keyword/None:     metatype[#Optional<FooStruct>.metatype#]{{$}}
// OPT_DOT_FOOSTRUCT-NEXT: End completions

// OPT_NO_DOT_INT: Begin completions, 4 items
// OPT_NO_DOT_INT-NEXT: Pattern/ExprSpecific: ![#Int#]{{$}}
// OPT_NO_DOT_INT-NEXT: Pattern/ExprSpecific: ?[#Int#]{{$}}
// OPT_NO_DOT_INT-NEXT: Decl/CurrNominal:     .getLogicValue()[#Bool#]{{$}}
// OPT_NO_DOT_INT-NEXT: Keyword/None:         .metatype[#Optional<Int64>.metatype#]{{$}}
// OPT_NO_DOT_INT-NEXT: End completions

// OPT_DOT_INT: Begin completions, 2 items
// OPT_DOT_INT-NEXT: Decl/CurrNominal: getLogicValue()[#Bool#]{{$}}
// OPT_DOT_INT-NEXT: Keyword/None:     metatype[#Optional<Int64>.metatype#]{{$}}
// OPT_DOT_INT-NEXT: End completions

// OPT_NO_DOT_OBJCCLASS: Begin completions, 4 items
// OPT_NO_DOT_OBJCCLASS-NEXT: Pattern/ExprSpecific: ![#ObjcClass#]{{$}}
// OPT_NO_DOT_OBJCCLASS-NEXT: Pattern/ExprSpecific: ?[#ObjcClass#]{{$}}
// OPT_NO_DOT_OBJCCLASS-NEXT: Decl/CurrNominal:     .getLogicValue()[#Bool#]{{$}}
// OPT_NO_DOT_OBJCCLASS-NEXT: Keyword/None:         .metatype[#Optional<ObjcClass>.metatype#]{{$}}
// OPT_NO_DOT_OBJCCLASS-NEXT: End completions

// OBCCLASS_MEMBERS_NO_DOT: Begin completions, 3 items
// OBCCLASS_MEMBERS_NO_DOT-NEXT: Decl/CurrNominal:  .instanceVar[#Int#]
// OBCCLASS_MEMBERS_NO_DOT-NEXT: Decl/CurrNominal:  .instanceFunc()[#ObjcClass#]
// OBCCLASS_MEMBERS_NO_DOT-NEXT: Keyword/None:      .metatype[#ObjcClass.metatype#]
// OBCCLASS_MEMBERS_NO_DOT-NEXT: End completions

// OBCCLASS_MEMBERS_DOT: Begin completions, 3 items
// OBCCLASS_MEMBERS_DOT-NEXT: Decl/CurrNominal:  instanceVar[#Int#]
// OBCCLASS_MEMBERS_DOT-NEXT: Decl/CurrNominal:  instanceFunc()[#ObjcClass#]
// OBCCLASS_MEMBERS_DOT-NEXT: Keyword/None:      metatype[#ObjcClass.metatype#]
// OBCCLASS_MEMBERS_DOT-NEXT: End completions

//===---
//===--- Tests.
//===---

func testOptionalVar1(a: Optional<FooStruct>) {
  a#^OPT_NO_DOT_1^#
}

func testOptionalVar2(a: Optional<FooStruct>) {
  a.#^OPT_DOT_1^#
}

func testOptionalReturnValue1() {
  returnsOptional()#^OPT_NO_DOT_2^#
}

func testOptionalReturnValue2() {
  returnsOptional().#^OPT_DOT_2^#
}

func testDynamicLookup1(a: DynamicLookup) {
  a.instanceVar#^OPT_NO_DOT_3^#
}

func testDynamicLookup2(a: DynamicLookup) {
  a.instanceVar.#^OPT_DOT_3^#
}

func testDynamicLookup3(a: DynamicLookup) {
  a.instanceFunc!()#^OPT_DL_NO_DOT_1^#
}

func testDynamicLookup4(a: DynamicLookup) {
  a.instanceFunc?()#^OPT_DL_NO_DOT_2^#
}

func testDynamicLookup5(a: DynamicLookup) {
  a.instanceFunc!().#^OPT_DL_DOT_1^#
}

func testDynamicLookup6(a: DynamicLookup) {
  a.instanceFunc?().#^OPT_DL_DOT_2^#
}

func testDynamicLookup7(a: DynamicLookup) {
  (a.instanceFunc?())#^OPT_DL_FORCE_RETURN_OPTIONAL_1^#
}

