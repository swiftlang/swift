// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

class A {
  @objc func a() { } // expected-note{{method 'a()' declared here}}

  @objc var prop: Int // expected-note{{setter for 'prop' declared here}}

  @objc init(prop: Int) { self.prop = prop } // expected-note{{initializer 'init(prop:)' declared here}}

  @objc subscript (i: Int) -> AnyObject { 
    get { return self }
    set { } // expected-note{{subscript setter declared here}}
  }
}

class B : A {
  var x: Int

  init(x: Int) { 
    self.x = x
    super.init(prop: x)
  }

  @objc(a) func f() { } // expected-error{{method 'f()' with Objective-C selector 'a' conflicts with method 'a()' from superclass 'A' with the same Objective-C selector}}

  @objc(initWithProp:) func initializeWithProp(_ prop: Int) { } // expected-error{{method 'initializeWithProp' with Objective-C selector 'initWithProp:' conflicts with initializer 'init(prop:)' from superclass 'A' with the same Objective-C selector}}

  @objc(setProp:) func setProperty(_ prop: Int) { } // expected-error{{method 'setProperty' with Objective-C selector 'setProp:' conflicts with setter for 'prop' from superclass 'A' with the same Objective-C selector}}

  @objc(setObject:atIndexedSubscript:) func doSet(_ x: AnyObject, y: Int) { } // expected-error{{method 'doSet(_:y:)' with Objective-C selector 'setObject:atIndexedSubscript:' conflicts with subscript setter from superclass 'A' with the same Objective-C selector}}
}
