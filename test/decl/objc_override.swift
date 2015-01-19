// RUN: %target-parse-verify-swift

class A {
  @objc func a() { } // expected-note{{Objective-C method 'a' defined by method 'a()' here}}

  @objc var prop: Int // expected-note{{Objective-C method 'setProp:' defined by setter for 'prop' here}}

  @objc init(prop: Int) { self.prop = prop } // expected-note{{Objective-C method 'initWithProp:' defined by initializer 'init(prop:)' here}}

  @objc subscript (i: Int) -> AnyObject { 
    get { return self }
    set { } // expected-note{{Objective-C method 'setObject:atIndexedSubscript:' defined by subscript setter here}}
  }
}

class B : A {
  var x: Int

  init(x: Int) { 
    self.x = x
    super.init(prop: x)
  }

  @objc(a) func f() { } // expected-error{{method 'f()' overrides Objective-C method 'a' from superclass 'A'}}

  @objc(initWithProp:) func initializeWithProp(prop: Int) { } // expected-error{{method 'initializeWithProp' overrides Objective-C method 'initWithProp:' from superclass 'A'}}

  @objc(setProp:) func setProperty(prop: Int) { } // expected-error{{method 'setProperty' overrides Objective-C method 'setProp:' from superclass 'A'}}

  @objc(setObject:atIndexedSubscript:) func doSet(x: AnyObject, y: Int) { } // expected-error{{method 'doSet(_:y:)' overrides Objective-C method 'setObject:atIndexedSubscript:' from superclass 'A'}}
}
