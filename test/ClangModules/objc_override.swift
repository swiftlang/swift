// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import Foundation
import ObjCParseExtras

class MyArray : NSArray {
  func setBoolProperty(x: Bool) { } // expected-error{{method 'setBoolProperty' overrides Objective-C method 'setBoolProperty:' from superclass 'NSArray'}}

  @objc(objectAtIndexedSubscript:)
  func getObjectAt(i: Int) { } // expected-error{{method 'getObjectAt' overrides Objective-C method 'objectAtIndexedSubscript:' from superclass 'NSArray'}}
}

class SomeCellSub1 : SomeCell {
  // FIXME: We should inherit the selector name here.
  init(string: String) { super.init(string: string) } // expected-error{{overriding declaration requires an 'override' keyword}}{{3-3=override }}
  // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('initWithString:' vs. 'initString:')}}{{3-3=@objc(initString:) }}

  // okay: should not conflict
  func initWithString(string: String) { }

  var enabled: Bool { // expected-error{{overriding declaration requires an 'override' keyword}}
    get { return super.enabled }
    // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('enabled' vs. 'isEnabled')}}{{5-5=@objc(isEnabled) }}
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub2 : SomeCell {
  // FIXME: We should inherit the selector name here.
  override init(string: String) { super.init(string: string) }
  // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('initWithString:' vs. 'initString:')}}{{3-3=@objc(initString:) }}

  // okay: should not conflict
  func initWithString(string: String) { }

  override var enabled: Bool {
    get { return super.enabled }
    // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('enabled' vs. 'isEnabled')}}{{5-5=@objc(isEnabled) }}
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub3 : SomeCell {
  @objc(initString:)
  override init(string: String) { super.init(string: string) }

  // okay: should not conflict
  func initWithString(string: String) { }

  override var enabled: Bool {
    @objc(isEnabled) get { return super.enabled }
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub4 : SomeCell {
  // FIXME: We should inherit the selector name here.
  @objc
  override init(string: String) { super.init(string: string) }
  // expected-error@-2{{Objective-C method has a different selector from the method it overrides ('initWithString:' vs. 'initString:')}}{{8-8=(initString:)}}

  // okay: should not conflict
  func initWithString(string: String) { }

  override var enabled: Bool {
    @objc get { return super.enabled }
    // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('enabled' vs. 'isEnabled')}}{{10-10=(isEnabled)}}
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

class SomeCellSub5 : SomeCell {
  @objc(initWithString:) // expected-error{{Objective-C method has a different selector from the method it overrides ('initWithString:' vs. 'initString:')}}{{9-24=initString:}}
  override init(string: String) { super.init(string: string) }

  override var enabled: Bool {
    @objc(wasEnabled) get { return super.enabled }
    // expected-error@-1{{Objective-C method has a different selector from the method it overrides ('wasEnabled' vs. 'isEnabled')}}{{11-21=isEnabled}}
  }

  @objc(enabled)
  func otherIsEnabled() { } // should not conflict
}

