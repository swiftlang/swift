// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify -verify-ignore-unknown

// REQUIRES: objc_interop

import Foundation
import ObjCParseExtras

class MyArray : DummyClass {
  @objc func setBoolProperty(_ x: Bool) { } // expected-error{{method 'setBoolProperty' with Objective-C selector 'setBoolProperty:' conflicts with setter for 'boolProperty' from superclass 'DummyClass' with the same Objective-C selector}}

  @objc(objectAtIndexedSubscript:)
  func getObjectAt(_ i: Int) { } // expected-error{{method 'getObjectAt' with Objective-C selector 'objectAtIndexedSubscript:' conflicts with method 'objectAtIndexedSubscript' from superclass 'DummyClass' with the same Objective-C selector}}
}

class SomeCellSub1 : SomeCell {
  init(string: String) { super.init(string: string) } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}

  // okay: should not conflict
  @objc func initWithString(_ string: String) { }

  var isEnabled: Bool { // expected-error{{overriding declaration requires an 'override' keyword}}
    get { return super.isEnabled }
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub2 : SomeCell {
  override init(string: String) { super.init(string: string) }

  // okay: should not conflict
  func initWithString(_ string: String) { }

  override var isEnabled: Bool {
    get { return super.isEnabled }
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub3 : SomeCell {
  @objc(initString:)
  override init(string: String) { super.init(string: string) }

  // okay: should not conflict
  func initWithString(_ string: String) { }

  override var isEnabled: Bool {
    @objc(isEnabled) get { return super.isEnabled }
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub4 : SomeCell {
  @objc
  override init(string: String) { super.init(string: string) }

  // okay: should not conflict
  func initWithString(_ string: String) { }

  override var isEnabled: Bool {
    @objc get { return super.isEnabled }
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub5 : SomeCell {
  @objc(initWithString:) // expected-error{{Objective-C method has a different selector from the method it overrides ('initWithString:' vs. 'initString:')}}{{9-24=initString:}}
  override init(string: String) { super.init(string: string) }

  override var isEnabled: Bool {
    @objc(wasEnabled) get { return super.isEnabled }
    // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('wasEnabled' vs. 'isEnabled')}}{{11-21=isEnabled}}
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class FailSub : FailBase {
  override init(value: Int) { try! super.init(value: value) } // expected-error {{overriding a throwing @objc initializer with a non-throwing initializer is not supported}}
  override class func processValue() {} // expected-error {{overriding a throwing @objc method with a non-throwing method is not supported}}
}

class CallbackSubA : CallbackBase {
  override func perform(handler: () -> Void) {} // expected-error {{method does not override any method from its superclass}}
  // expected-note@-1 {{type does not match superclass instance method with type '(@escaping () -> Void) -> Void'}}
  override func perform(optHandler: () -> Void) {} // expected-error {{method does not override any method from its superclass}}
  override func perform(nonescapingHandler: () -> Void) {}
  override func perform(optNonescapingHandler: () -> Void) {} // expected-error {{cannot override instance method parameter of type '(() -> Void)?' with non-optional type '() -> Void'}}
}
class CallbackSubB : CallbackBase {
  override func perform(handler: (() -> Void)?) {}
  override func perform(optHandler: (() -> Void)?) {}
  override func perform(nonescapingHandler: (() -> Void)?) {} // expected-error {{method does not override any method from its superclass}}
  override func perform(optNonescapingHandler: (() -> Void)?) {}
}
class CallbackSubC : CallbackBase {
  override func perform(handler: @escaping () -> Void) {}
  override func perform(optHandler: @escaping () -> Void) {} // expected-error {{cannot override instance method parameter of type '(() -> Void)?' with non-optional type '() -> Void'}}
  override func perform(nonescapingHandler: @escaping () -> Void) {} // expected-error {{method does not override any method from its superclass}}
  override func perform(optNonescapingHandler: @escaping () -> Void) {} // expected-error {{method does not override any method from its superclass}}
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: overridden declaration is here
// <unknown>:0: error: unexpected note produced: setter for 'boolProperty' declared here
