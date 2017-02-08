// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/mixed-target/ -module-name Mixed -import-underlying-module -typecheck %s -verify -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/mixed-target/ -module-name Mixed -import-underlying-module -emit-ir %S/../../Inputs/empty.swift - | %FileCheck -check-prefix=CHECK-AUTOLINK %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/mixed-target/ -module-name WrongName -import-underlying-module -typecheck %s  -disable-objc-attr-requires-foundation-module 2>&1 | %FileCheck -check-prefix=CHECK-WRONG-NAME %s

// REQUIRES: objc_interop

// CHECK-AUTOLINK: !{{[0-9]+}} = !{i32 {{[0-9]+}}, !"Linker Options", ![[LINK_LIST:[0-9]+]]}
// CHECK-AUTOLINK: ![[LINK_LIST]] = !{
// CHECK-AUTOLINK-NOT: metadata !"-framework", metadata !"Mixed"

// CHECK-WRONG-NAME: underlying Objective-C module 'WrongName' not found

@objc class ForwardClass : NSObject {
}

@objc protocol ForwardProto : NSObjectProtocol {
}
@objc class ForwardProtoAdopter : NSObject, ForwardProto {
}

@objc class PartialBaseClass {
}
@objc class PartialSubClass : NSObject {
}

func testCFunction() {
  doSomething(ForwardClass())
  doSomethingProto(ForwardProtoAdopter())
  doSomethingPartialBase(PartialBaseClass())
  doSomethingPartialSub(PartialSubClass())
}


class Derived : Base {
  override func safeOverride(_ arg: NSObject) -> ForwardClass { // no-warning
    return ForwardClass()
  }

  override func unsafeOverrideParam(_ arg: ForwardClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }

  override func unsafeOverrideReturn(_ arg: ForwardClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }

  override func safeOverridePartialSub(_ arg: NSObject?) -> PartialSubClass { // no-warning
    return PartialSubClass()
  }

  override func unsafeOverridePartialSubParam(_ arg: PartialSubClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }

  override func unsafeOverridePartialSubReturn(_ arg: PartialSubClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }
}

func testMethod(_ container: Base, input: ForwardClass, inputProto: ForwardProto, inputPartial: PartialSubClass) {
  _ = container.unsafeOverrideReturn(input) as ForwardClass// no-warning
  _ = container.unsafeOverrideProtoReturn(inputProto) as ForwardProto // no-warning
  _ = container.unsafeOverridePartialSubReturn(inputPartial) as PartialSubClass// no-warning
}


class ProtoConformer : ForwardClassUser {
  @objc func consumeForwardClass(_ arg: ForwardClass) {}

  @objc var forward = ForwardClass()
}

func testProtocolWrapper(_ conformer: ForwardClassUser) {
  conformer.consumeForwardClass(conformer.forward)
}
testProtocolWrapper(ProtoConformer())

func testDeclsNestedInObjCContainers() {
  let _: NameInInterface = 0
  let _: NameInProtocol = 0
  let _: NameInCategory = 0
}
