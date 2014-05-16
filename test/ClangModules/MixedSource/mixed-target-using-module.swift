// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -module-cache-path %t -F %S/Inputs/mixed-target/ -module-name Mixed -import-underlying-module -parse %s -verify
// RUN: not %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -module-cache-path %t -F %S/Inputs/mixed-target/ -module-name WrongName -import-underlying-module -parse %s 2>&1 | FileCheck -check-prefix=CHECK-WRONG-NAME %s

// If this test fails for you, you probably aren't using the correct Clang repository.

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

  override func unsafeOverrideParam(arg: ForwardClass) -> NSObject { // expected-error{{incompatible type}}
    return arg
  }

  override func unsafeOverrideReturn(arg: ForwardClass) -> NSObject { // expected-error{{incompatible type}}
    return arg
  }

  // FIXME: This override is actually safe.
  override func safeOverrideProto(arg: NSObjectProtocol) -> ForwardProtoAdopter { // expected-error{{incompatible type}}
    return ForwardProtoAdopter()
  }

  override func unsafeOverrideParam(arg: ForwardProtoAdopter) -> NSObjectProtocol { // expected-error{{incompatible type}}
    return arg
  }

  override func unsafeOverrideReturn(arg: ForwardProtoAdopter) -> NSObjectProtocol { // expected-error{{incompatible type}}
    return arg
  }

  override func safeOverridePartialSub(arg: NSObject?) -> PartialSubClass { // no-warning
    return PartialSubClass()
  }

  override func unsafeOverridePartialSubParam(arg: PartialSubClass) -> NSObject { // expected-error{{incompatible type}}
    return arg
  }

  override func unsafeOverridePartialSubReturn(arg: PartialSubClass) -> NSObject { // expected-error{{incompatible type}}
    return arg
  }
}

func testMethod(container: Base, input: ForwardClass, inputProto: ForwardProto, inputPartial: PartialSubClass) {
  let output: ForwardClass = container.unsafeOverrideReturn(input) // no-warning
  let outputProto: ForwardProto = container.unsafeOverrideProtoReturn(inputProto) // no-warning
  let outputPartial: PartialSubClass = container.unsafeOverridePartialSubReturn(inputPartial) // no-warning
}


class ProtoConformer : ForwardClassUser {
  func consumeForwardClass(arg: ForwardClass) {}

  var forward = ForwardClass()
}

func testProtocolWrapper(conformer: ForwardClassUser) {
  conformer.consumeForwardClass(conformer.forward)
}
testProtocolWrapper(ProtoConformer())

