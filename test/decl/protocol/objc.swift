// RUN: %target-parse-verify-swift

// Test requirements and conformance for Objective-C protocols.

@objc class ObjCClass { }

@objc protocol P1 {
  func method1() // expected-note{{protocol requires function 'method1()' with type '() -> ()'}}

  var property1: ObjCClass { get } // expected-note{{protocol requires property 'property1' with type 'ObjCClass'}}
  var property2: ObjCClass { get set } // expected-note{{protocol requires property 'property2' with type 'ObjCClass'}}
}

@objc class C1 : P1 { // expected-error{{type 'C1' does not conform to protocol 'P1'}}
  @objc(method1renamed)
  func method1() { // expected-note{{Objective-C method 'method1renamed' provided by method 'method1()' does not match the requirement's selector ('method1')}}
  }

  var property1: ObjCClass {
    @objc(getProperty1) get { return ObjCClass() } // expected-note{{Objective-C method 'getProperty1' provided by getter for 'property1' does not match the requirement's selector ('property1')}}
  }

  var property2: ObjCClass {
    get { return ObjCClass() }
    @objc(setProperty2Please:) set { } // expected-note{{Objective-C method 'setProperty2Please:' provided by setter for 'property2' does not match the requirement's selector ('setProperty2:')}}
  }
}

// Complain about optional requirements that aren't satisfied
// according to Swift, but would conflict in Objective-C.
@objc protocol OptP1 {
  optional func method() // expected-note 2{{requirement 'method()' declared here}}

  optional var property1: ObjCClass { get } // expected-note 2{{requirement 'property1' declared here}}
  optional var property2: ObjCClass { get set } // expected-note{{requirement 'property2' declared here}}
}

@objc class OptC1a : OptP1 { // expected-note 3{{class 'OptC1a' declares conformance to protocol 'OptP1' here}}
  @objc(method) func otherMethod() { } // expected-error{{Objective-C method 'method' provided by method 'otherMethod()' conflicts with optional requirement method 'method()' in protocol 'OptP1'}}

  var otherProp1: ObjCClass {
    @objc(property1) get { return ObjCClass() } // expected-error{{Objective-C method 'property1' provided by getter for 'otherProp1' conflicts with optional requirement getter for 'property1' in protocol 'OptP1'}}
  }

  var otherProp2: ObjCClass {
    get { return ObjCClass() }
    @objc(setProperty2:) set { } // expected-error{{Objective-C method 'setProperty2:' provided by setter for 'otherProp2' conflicts with optional requirement setter for 'property2' in protocol 'OptP1'}}
  }
}

@objc class OptC1b : OptP1 { // expected-note 2{{class 'OptC1b' declares conformance to protocol 'OptP1' here}}
  @objc(property1) func someMethod() { } // expected-error{{Objective-C method 'property1' provided by method 'someMethod()' conflicts with optional requirement getter for 'property1' in protocol 'OptP1'}}

  var someProp: ObjCClass {
    @objc(method) get { return ObjCClass() } // expected-error{{Objective-C method 'method' provided by getter for 'someProp' conflicts with optional requirement method 'method()' in protocol 'OptP1'}}
  }
}
