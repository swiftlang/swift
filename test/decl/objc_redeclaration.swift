// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

@objc class Redecl1 { // expected-note{{Objective-C method 'dealloc' previously declared by implicit deinitializer here}}
  @objc init() { } // expected-note{{Objective-C method 'init' previously declared by initializer 'init()' here}}

  @objc
  func method1() { } // expected-note 2{{Objective-C method 'method1' previously declared by method 'method1()' here}}

  @objc var value: Int // expected-note{{Objective-C method 'setValue:' previously declared by setter for 'value' here}}

  @objc(wibble) var other: Int
  // expected-note@-1{{Objective-C method 'setWibble:' previously declared by setter for 'other' here}}
  // expected-note@-2{{Objective-C method 'wibble' previously declared by getter for 'other' here}}
}

extension Redecl1 {
  @objc(method1)
  func method1_alias() { } // expected-error{{method 'method1_alias()' redeclares Objective-C method 'method1'}}
}

extension Redecl1 {
  @objc var method1_var_alias: Int {
    @objc(method1) get { return 5 } // expected-error{{getter for 'method1_var_alias' redeclares Objective-C method 'method1'}}

    @objc(method2:) set { } // expected-note{{Objective-C method 'method2:' previously declared by setter for 'method1_var_alias' here}}
  }

  @objc subscript (i: Int) -> Redecl1 {
    get { return self } // expected-note{{Objective-C method 'objectAtIndexedSubscript:' previously declared by subscript getter here}}
    set { }
  }
}

extension Redecl1 {
  @objc
  func method2(x: Int) { } // expected-error{{method 'method2' redeclares Objective-C method 'method2:'}}

  @objc(objectAtIndexedSubscript:)
  func indexed(x: Int) { } // expected-error{{method 'indexed' redeclares Objective-C method 'objectAtIndexedSubscript:'}}

  @objc(init)
  func initialize() { } // expected-error{{method 'initialize()' redeclares Objective-C method 'init'}}

  @objc
  func dealloc() { } // expected-error{{method 'dealloc()' redeclares Objective-C method 'dealloc'}}

  @objc func setValue(x: Int) { } // expected-error{{method 'setValue' redeclares Objective-C method 'setValue:'}}
}

extension Redecl1 {
  @objc func setWibble(other: Int) { } // expected-error{{method 'setWibble' redeclares Objective-C method 'setWibble:'}}
  @objc func wibble() -> Int { return 0 } // expected-error{{method 'wibble()' redeclares Objective-C method 'wibble'}}
}
