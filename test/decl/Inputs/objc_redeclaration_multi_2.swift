extension Redecl1 {
  @objc(method1)
  func method1_alias() { } // expected-error{{method 'method1_alias()' redeclares Objective-C method 'method1'}}

  @objc(init)
  func initialize() { } // expected-error{{method 'initialize()' redeclares Objective-C method 'init'}}
}

@objc class Redecl2 {
  @objc init() { } // expected-error{{initializer 'init()' redeclares Objective-C method 'init'}}

  @objc
  func method1() { } // expected-error{{method 'method1()' redeclares Objective-C method 'method1'}}
}
