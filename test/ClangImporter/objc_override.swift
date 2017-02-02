// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify -verify-ignore-unknown

// REQUIRES: objc_interop

import Foundation
import ObjCParseExtras

class MyArray : DummyClass {
  func setBoolProperty(_ x: Bool) { } // expected-error{{method 'setBoolProperty' with Objective-C selector 'setBoolProperty:' conflicts with setter for 'boolProperty' from superclass 'DummyClass' with the same Objective-C selector}}

  @objc(objectAtIndexedSubscript:)
  func getObjectAt(_ i: Int) { } // expected-error{{method 'getObjectAt' with Objective-C selector 'objectAtIndexedSubscript:' conflicts with method 'objectAtIndexedSubscript' from superclass 'DummyClass' with the same Objective-C selector}}
}

class SomeCellSub1 : SomeCell {
  init(string: String) { super.init(string: string) } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}

  // okay: should not conflict
  func initWithString(_ string: String) { }

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

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: overridden declaration is here
// <unknown>:0: error: unexpected note produced: setter for 'boolProperty' declared here
