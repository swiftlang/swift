// REQUIRES: objc_interop

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=UN_OPT_NO_DOT_3 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=UN_OPT_NO_DOT_INT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=UN_OPT_DOT_3 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=UN_OPT_DOT_INT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DL_NO_DOT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_NO_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DL_NO_DOT_2 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_NO_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DL_DOT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DL_DOT_2 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OBJCCLASS_MEMBERS_DOT < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_DL_FORCE_RETURN_OPTIONAL_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_NO_DOT_OBJCCLASS < %t.opt.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-objc-attr-requires-foundation-module -code-completion-token=OPT_CAST_AS_RESULT_1 > %t.opt.txt
// RUN: %FileCheck %s -check-prefix=OPT_NO_DOT_OBJCCLASS < %t.opt.txt

@objc
class ObjcClass {
  @objc var instanceVar: Int = 0
  @objc func instanceFunc() -> ObjcClass { return self }
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

// UN_OPT_NO_DOT_INT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: ?.nonzeroBitCount[#Int#]{{; name=.+$}}

// UN_OPT_DOT_INT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem/Erase[1]: ?.nonzeroBitCount[#Int#]{{; name=.+$}}

// OBJCCLASS_MEMBERS_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .instanceVar[#Int#]
// OBJCCLASS_MEMBERS_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .instanceFunc()[#ObjcClass#]
// OBJCCLASS_MEMBERS_NO_DOT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]/IsSystem: === {#AnyObject?#}[#Bool#]
// OBJCCLASS_MEMBERS_NO_DOT-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]/IsSystem: !== {#AnyObject?#}[#Bool#]
// OBJCCLASS_MEMBERS_NO_DOT-DAG: Keyword[self]/CurrNominal: .self[#ObjcClass#]; name=self

// OBJCCLASS_MEMBERS_DOT-DAG: Keyword[self]/CurrNominal: self[#ObjcClass#]; name=self
// OBJCCLASS_MEMBERS_DOT-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]
// OBJCCLASS_MEMBERS_DOT-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#ObjcClass#]

// OPT_NO_DOT_OBJCCLASS-DAG: Decl[InstanceVar]/CurrNominal:    ?.instanceVar[#Int#]{{; name=.+$}}
// OPT_NO_DOT_OBJCCLASS-DAG: Decl[InstanceMethod]/CurrNominal: ?.instanceFunc()[#ObjcClass#]{{; name=.+$}}
