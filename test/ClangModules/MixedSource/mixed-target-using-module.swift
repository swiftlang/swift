// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/mixed-target/ -module-name Mixed -import-underlying-module -parse %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/mixed-target/ -module-name Mixed -import-underlying-module -emit-ir %S/../../Inputs/empty.swift - | FileCheck -check-prefix=CHECK-AUTOLINK %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/mixed-target/ -module-name WrongName -import-underlying-module -parse %s 2>&1 | FileCheck -check-prefix=CHECK-WRONG-NAME %s

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
  override func safeOverride(arg: NSObject) -> ForwardClass { // no-warning
    return ForwardClass()
  }

  override func unsafeOverrideParam(arg: ForwardClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }

  override func unsafeOverrideReturn(arg: ForwardClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }

  override func safeOverridePartialSub(arg: NSObject?) -> PartialSubClass { // no-warning
    return PartialSubClass()
  }

  override func unsafeOverridePartialSubParam(arg: PartialSubClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }

  override func unsafeOverridePartialSubReturn(arg: PartialSubClass) -> NSObject { // expected-error{{does not override}}
    return arg
  }
}

func testMethod(container: Base, input: ForwardClass, inputProto: ForwardProto, inputPartial: PartialSubClass) {
  let output: ForwardClass = container.unsafeOverrideReturn(input) // no-warning
  let outputProto: ForwardProto = container.unsafeOverrideProtoReturn(inputProto) // no-warning
  let outputPartial: PartialSubClass = container.unsafeOverridePartialSubReturn(inputPartial) // no-warning
}


class ProtoConformer : ForwardClassUser {
  @objc func consumeForwardClass(arg: ForwardClass) {}

  @objc var forward = ForwardClass()
}

func testProtocolWrapper(conformer: ForwardClassUser) {
  conformer.consumeForwardClass(conformer.forward)
}
testProtocolWrapper(ProtoConformer())

