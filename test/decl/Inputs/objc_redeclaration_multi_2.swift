extension Redecl1 {
  @objc(method1)
  func method1_alias() { } // expected-error{{method 'method1_alias()' with Objective-C selector 'method1' conflicts with method 'method1()' with the same Objective-C selector}}

  @objc(init)
  func initialize() { } // expected-error{{method 'initialize()' with Objective-C selector 'init' conflicts with initializer 'init()' with the same Objective-C selector}}

  @objc func method2() { } // expected-note{{method 'method2()' declared here}}
}

@objc class Redecl2 {
  @objc init() { } // expected-note{{initializer 'init()' declared here}}

  @objc
  func method1() { } // expected-note{{method 'method1()' declared here}}
}
