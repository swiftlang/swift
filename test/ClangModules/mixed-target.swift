// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -module-cache-path %t -I %S/Inputs/mixed-target/ -module-name Mixed -parse %s -verify

import Mixed
import ObjectiveC

@objc class ForwardClass : NSObject {
}

func testCFunction() {
  doSomething(ForwardClass())
}


class Derived : Base {
  @override func safeOverride(arg: NSObject) -> ForwardClass { // no-warning
    return ForwardClass()
  }

  @override func unsafeOverrideParam(arg: ForwardClass) -> NSObject { // expected-error{{incompatible type}}
    return arg
  }

  @override func unsafeOverrideReturn(arg: ForwardClass) -> NSObject { // expected-error{{incompatible type}}
    return arg
  }
}

func testMethod(container: Base, input: ForwardClass) {
  let output: ForwardClass = container.unsafeOverrideReturn(input) // no-warning
}


class ProtoConformer : UseForwardProto {
  func consumeForwardClass(arg: ForwardClass) {}

  var forward = ForwardClass()
}

func testProtocolWrapper(conformer: UseForwardProto) {
  conformer.consumeForwardClass(conformer.forward)
}
testProtocolWrapper(ProtoConformer())
