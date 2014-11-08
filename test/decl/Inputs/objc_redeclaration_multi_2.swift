extension Redecl1 {
  @objc(method1)
  func method1_alias() { } // expected-note{{Objective-C method 'method1' previously declared by method 'method1_alias()' here}}

  @objc(init)
  func initialize() { } // expected-note{{Objective-C method 'init' previously declared by method 'initialize()' here}}
}

@objc class Redecl2 {
  @objc init() { } // expected-error{{initializer 'init()' redeclares Objective-C method 'init'}}

  @objc
  func method1() { } // expected-error{{method 'method1()' redeclares Objective-C method 'method1'}}
}
