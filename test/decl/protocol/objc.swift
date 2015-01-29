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
