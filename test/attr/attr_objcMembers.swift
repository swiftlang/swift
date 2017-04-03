// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

@objcMembers
class SomeClassWithObjCMembers {
  func foo() { }
}

extension SomeClassWithObjCMembers {
  var bar: NSObject? { get { return nil } set { } }
}

// @objcMembers is inherited
class SubClassOfSomeClassWithObjCMembers : SomeClassWithObjCMembers {
  func baz() { }

  // Don't complain about things not expressible in Objective-C.
  func notForObjC() -> (Int, Int) { return (0, 0) }
}

extension SubClassOfSomeClassWithObjCMembers {
  func wibble() { }
}

// @objc should be inferred for everything referenced here.
func selectorTest() {
  _ = #selector(SomeClassWithObjCMembers.foo)
  _ = #selector(getter: SomeClassWithObjCMembers.bar)
  _ = #selector(SubClassOfSomeClassWithObjCMembers.baz)
  _ = #selector(SubClassOfSomeClassWithObjCMembers.wibble)
}

@nonobjc extension SomeClassWithObjCMembers {
  func notInferredObjC() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
}

func selectorTestFail() {
  _ = #selector(SomeClassWithObjCMembers.notInferredObjC) // expected-error{{argument of '#selector' refers to instance method 'notInferredObjC()' that is not exposed to Objective-C}}
}
