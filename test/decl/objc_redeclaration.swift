// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -module-name ZZZ %s -disable-objc-attr-requires-foundation-module -verify -verify-ignore-unknown

import Foundation

// REQUIRES: objc_interop

@objc class Redecl1 { // expected-note{{implicit deinitializer declared here}}
  @objc init() { } // expected-note{{initializer 'init()' declared here}}

  @objc
  func method1() { } // expected-note 2{{method 'method1()' declared here}}

  @objc var value: Int // expected-note{{setter for 'value' declared here}}

  @objc(wibble) var other: Int
  // expected-note@-1{{getter for 'other' declared here}}
  // expected-note@-2{{setter for 'other' declared here}}
}

extension Redecl1 {
  @objc(method1)
  func method1_alias() { } // expected-error{{method 'method1_alias()' with Objective-C selector 'method1' conflicts with method 'method1()' with the same Objective-C selector}}
}

extension Redecl1 {
  @objc var method1_var_alias: Int {
    @objc(method1) get { return 5 } // expected-error{{getter for 'method1_var_alias' with Objective-C selector 'method1' conflicts with method 'method1()' with the same Objective-C selector}}

    @objc(method2:) set { } // expected-note{{setter for 'method1_var_alias' declared here}}
  }

  @objc subscript (i: Int) -> Redecl1 {
    get { return self } // expected-note{{subscript getter declared here}}
    set { }
  }
}

extension Redecl1 {
  @objc
  func method2(_ x: Int) { } // expected-error{{method 'method2' with Objective-C selector 'method2:' conflicts with setter for 'method1_var_alias' with the same Objective-C selector}}

  @objc(objectAtIndexedSubscript:)
  func indexed(_ x: Int) { } // expected-error{{method 'indexed' with Objective-C selector 'objectAtIndexedSubscript:' conflicts with subscript getter with the same Objective-C selector}}

  @objc(init)
  func initialize() { } // expected-error{{method 'initialize()' with Objective-C selector 'init' conflicts with initializer 'init()' with the same Objective-C selector}}

  @objc
  func dealloc() { } // expected-error{{method 'dealloc()' with Objective-C selector 'dealloc' conflicts with implicit deinitializer with the same Objective-C selector}}

  @objc func setValue(_ x: Int) { } // expected-error{{method 'setValue' with Objective-C selector 'setValue:' conflicts with setter for 'value' with the same Objective-C selector}}
}

extension Redecl1 {
  @objc func setWibble(_ other: Int) { } // expected-error{{method 'setWibble' with Objective-C selector 'setWibble:' conflicts with setter for 'other' with the same Objective-C selector}}
  @objc func wibble() -> Int { return 0 } // expected-error{{method 'wibble()' with Objective-C selector 'wibble' conflicts with getter for 'other' with the same Objective-C selector}}
}

extension DummyClass {
  @objc func nsstringProperty2() -> Int { return 0 } // expected-error{{method 'nsstringProperty2()' with Objective-C selector 'nsstringProperty2' conflicts with getter for 'nsstringProperty2' with the same Objective-C selector}}
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'nsstringProperty2' previously declared here
