// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_NO_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_NO_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DOT_1_SPACES > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT_SPACES < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UN_OPT_NO_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=UN_OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UN_OPT_NO_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=UN_OPT_NO_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UN_OPT_NO_DOT_3 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=UN_OPT_NO_DOT_INT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UN_OPT_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=UN_OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UN_OPT_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=UN_OPT_DOT_FOOSTRUCT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UN_OPT_DOT_3 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=UN_OPT_DOT_INT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_NO_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_NO_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_NO_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_NO_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_DOT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_DOT_2 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_DL_FORCE_RETURN_OPTIONAL_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_OBJCCLASS < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPT_CAST_AS_RESULT_1 > %t.opt.txt
// RUN: FileCheck %s -check-prefix=OPT_NO_DOT_OBJCCLASS < %t.opt.txt

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

@objc
class ObjcClass {
  var instanceVar: Int = 0
  func instanceFunc() -> ObjcClass { return self }
}

// OPT_NO_DOT_FOOSTRUCT: Begin completions
// OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal: ?.instanceFunc()[#Void#]{{; name=.+$}}
// OPT_NO_DOT_FOOSTRUCT: End completions

// OPT_DOT_FOOSTRUCT: Begin completions
// OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal/Erase[1]:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal/Erase[1]: ?.instanceFunc()[#Void#]{{; name=.+$}}
// OPT_DOT_FOOSTRUCT: End completions

// OPT_DOT_FOOSTRUCT_SPACES: Begin completions
// OPT_DOT_FOOSTRUCT_SPACES-DAG: Decl[InstanceVar]/CurrNominal/Erase[3]:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_DOT_FOOSTRUCT_SPACES-DAG: Decl[InstanceMethod]/CurrNominal/Erase[3]: ?.instanceFunc()[#Void#]{{; name=.+$}}
// OPT_DOT_FOOSTRUCT_SPACES: End completions

// UN_OPT_NO_DOT_FOOSTRUCT: Begin completions
// UN_OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal:    .instanceVar[#Int#]{{; name=.+$}}
// UN_OPT_NO_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal: .instanceFunc()[#Void#]{{; name=.+$}}
// UN_OPT_NO_DOT_FOOSTRUCT: End completions

// UN_OPT_DOT_FOOSTRUCT: Begin completions
// UN_OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// UN_OPT_DOT_FOOSTRUCT-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Void#]{{; name=.+$}}
// UN_OPT_DOT_FOOSTRUCT: End completions

// UN_OPT_NO_DOT_INT: Begin completions
// UN_OPT_NO_DOT_INT-DAG: Decl[InstanceMethod]/CurrNominal: .successor()[#Int#]{{; name=.+$}}
// UN_OPT_NO_DOT_INT: End completions

// UN_OPT_DOT_INT: Begin completions
// UN_OPT_DOT_INT-DAG: Decl[InstanceMethod]/CurrNominal: successor()[#Int#]{{; name=.+$}}
// UN_OPT_DOT_INT: End completions

// OPT_NO_DOT_OBJCCLASS: Begin completions
// OPT_NO_DOT_OBJCCLASS-DAG: Decl[InstanceVar]/CurrNominal:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_NO_DOT_OBJCCLASS-DAG: Decl[InstanceMethod]/CurrNominal: ?.instanceFunc()[#ObjcClass#]{{; name=.+$}}
// OPT_NO_DOT_OBJCCLASS: End completions

// OBJCCLASS_MEMBERS_NO_DOT: Begin completions
// OBJCCLASS_MEMBERS_NO_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    .instanceVar[#Int#]
// OBJCCLASS_MEMBERS_NO_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: .instanceFunc()[#ObjcClass#]
// OBJCCLASS_MEMBERS_NO_DOT-NEXT: End completions

// OBJCCLASS_MEMBERS_DOT: Begin completions
// OBJCCLASS_MEMBERS_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]
// OBJCCLASS_MEMBERS_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#ObjcClass#]
// OBJCCLASS_MEMBERS_DOT-NEXT: End completions

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

func testAnyObject1(a: AnyObject) {
  a.instanceVar#^UN_OPT_NO_DOT_3^#
}

func testAnyObject2(a: AnyObject) {
  a.instanceVar.#^UN_OPT_DOT_3^#
}

func testAnyObject3(a: AnyObject) {
  a.instanceFunc!()#^OPT_DL_NO_DOT_1^#
}

func testAnyObject4(a: AnyObject) {
  a.instanceFunc?()#^OPT_DL_NO_DOT_2^#
}

func testAnyObject5(a: AnyObject) {
  a.instanceFunc!().#^OPT_DL_DOT_1^#
}

func testAnyObject6(a: AnyObject) {
  a.instanceFunc?().#^OPT_DL_DOT_2^#
}

func testAnyObject7(a: AnyObject) {
  (a.instanceFunc?())#^OPT_DL_FORCE_RETURN_OPTIONAL_1^#
}

func testAnyObject8(a: AnyObject) {
  (a as? ObjcClass)#^OPT_CAST_AS_RESULT_1^#
}
