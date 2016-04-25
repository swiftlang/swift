// RUN: %target-parse-verify-swift

// Test requirements and conformance for Objective-C protocols.

@objc class ObjCClass { }

@objc protocol P1 {
  func method1() // expected-note {{requirement 'method1()' declared here}}

  var property1: ObjCClass { get } // expected-note{{requirement 'property1' declared here}}
  var property2: ObjCClass { get set } // expected-note{{requirement 'property2' declared here}}
}

@objc class C1 : P1 {
  @objc(method1renamed)
  func method1() { // expected-error{{Objective-C method 'method1renamed' provided by method 'method1()' does not match the requirement's selector ('method1')}}{{9-23=method1}}
  }

  var property1: ObjCClass {
    @objc(getProperty1) get { // expected-error{{Objective-C method 'getProperty1' provided by getter for 'property1' does not match the requirement's selector ('property1')}}{{11-23=property1}}
      return ObjCClass()
    } 
  }

  var property2: ObjCClass {
    get { return ObjCClass() }
    @objc(setProperty2Please:) set { } // expected-error{{Objective-C method 'setProperty2Please:' provided by setter for 'property2' does not match the requirement's selector ('setProperty2:')}}{{11-30=setProperty2:}}
  }
}

class C1b : P1 {
  func method1() { } // expected-error{{non-'@objc' method 'method1()' does not satisfy requirement of '@objc' protocol 'P1'}}{{3-3=@objc }}
  var property1: ObjCClass = ObjCClass() // expected-error{{non-'@objc' property 'property1' does not satisfy requirement of '@objc' protocol 'P1'}}{{3-3=@objc }}
  var property2: ObjCClass = ObjCClass() // expected-error{{non-'@objc' property 'property2' does not satisfy requirement of '@objc' protocol 'P1'}}{{3-3=@objc }}
}

@objc protocol P2 {
  @objc(methodWithInt:withClass:)
  func method(_: Int, class: ObjCClass) // expected-note{{'method(_:class:)' declared here}}

  var empty: Bool { @objc(checkIfEmpty) get } // expected-note{{requirement 'empty' declared here}}
}

class C2a : P2 {
  func method(_: Int, class: ObjCClass) { } // expected-error{{non-'@objc' method 'method(_:class:)' does not satisfy requirement of '@objc' protocol 'P2'}}{{3-3=@objc(methodWithInt:withClass:) }}

  var empty: Bool { // expected-error{{non-'@objc' property 'empty' does not satisfy requirement of '@objc' protocol 'P2'}}{{3-3=@objc }}
    get { }
  }
}

class C2b : P2 {
  @objc func method(_: Int, class: ObjCClass) { } // expected-error{{Objective-C method 'method:class:' provided by method 'method(_:class:)' does not match the requirement's selector ('methodWithInt:withClass:')}}{{8-8=(methodWithInt:withClass:)}}

  @objc var empty: Bool {
    @objc get { } // expected-error{{Objective-C method 'empty' provided by getter for 'empty' does not match the requirement's selector ('checkIfEmpty')}}{{10-10=(checkIfEmpty)}}
  }
}

@objc protocol P3 {
  optional func doSomething(x: Int)
  optional func doSomething(y: Int)
}

class C3a : P3 {
  @objc func doSomething(x: Int) { }
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
  // expected-note@-1{{rename method to match requirement 'method()'}}{{22-33=method}}

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

// rdar://problem/19879598
@objc protocol Foo {
  init() // expected-note{{requirement 'init()' declared here}}
}

class Bar: Foo {
  required init() {} // expected-error{{non-'@objc' initializer 'init()' does not satisfy requirement of '@objc' protocol 'Foo'}}{{3-3=@objc }}
}
