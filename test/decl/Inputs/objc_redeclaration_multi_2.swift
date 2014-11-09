extension Redecl1 {
  @objc(method1)
  func method1_alias() { } // expected-error{{method 'method1_alias()' redeclares Objective-C method 'method1'}}

  @objc(init)
  func initialize() { } // expected-error{{method 'initialize()' redeclares Objective-C method 'init'}}

  @objc func method2() { } // expected-error{{method 'method2()' redeclares Objective-C method 'method2'}}
}

@objc class Redecl2 {
  @objc init() { } // expected-note{{Objective-C method 'init' previously declared by initializer 'init()' here}}

  @objc
  func method1() { } // expected-note{{Objective-C method 'method1' previously declared by method 'method1()' here}}
}
