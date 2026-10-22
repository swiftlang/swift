// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -enable-objc-interop -import-objc-header %S/Inputs/implicit_force_unwrap.h %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -enable-objc-interop -import-objc-header %S/Inputs/implicit_force_unwrap.h %s -Wwarning ImplicitForceUnwrap -verify -verify-additional-prefix enabled-

// REQUIRES: objc_interop

// The ImplicitForceUnwrap warnings are off by default, so the first RUN line
// expects no diagnostics.

import Foundation

class Derived: IUOBase {
  override func use(_ object: NSObject, other: NSObject!) {} // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type 'NSObject?' in override of instance method 'use(_:other:)'}}{{39-39=!}}

  override func take(_ block: @escaping () -> Void) {} // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type '(() -> Void)?' in override of instance method 'take'}}{{31-41=(}}{{51-51=)!}}

  // Results are covariant, so returning a non-optional value is fine.
  override func produceObject() -> NSObject { NSObject() }

  override subscript(key: NSObject) -> NSObject { NSObject() } // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type 'NSObject?' in override of subscript 'subscript(_:)'}}{{35-35=!}}

  override init(object: NSObject) { super.init() } // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type 'NSObject?' in override of initializer 'init(object:)'}}{{33-33=!}}
}

class DerivedOK: IUOBase {
  override func use(_ object: NSObject!, other: NSObject?) {}
  override func take(_ block: (() -> Void)!) {}
  override init(object: NSObject?) { super.init() }
}

class Conformer: NSObject, IUOProto {
  func requireUse(_ object: sending NSObject) {} // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional parameter of type 'NSObject?' in witness for instance method 'requireUse'}}{{45-45=!}}

  func requireProduceObject() -> NSObject { NSObject() }

  var settableProp: NSObject = NSObject() // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional value of type 'NSObject?' in witness for property 'settableProp'}}{{29-29=!}}

  var inferredProp = NSObject() // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional value of type 'NSObject?' in witness for property 'inferredProp'}}{{none}}

  // A read-only requirement never passes a value to the witness.
  var readOnlyProp: NSObject = NSObject()
}

class ConformerOK: NSObject, IUOProto {
  func requireUse(_ object: NSObject!) {}
  func requireProduceObject() -> NSObject! { NSObject() }
  var settableProp: NSObject? = NSObject()
  var inferredProp: NSObject! = NSObject()
  var readOnlyProp: NSObject = NSObject()
}
