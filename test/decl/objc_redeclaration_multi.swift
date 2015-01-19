// RUN: %target-swift-frontend -parse -disable-objc-attr-requires-foundation-module %s %S/Inputs/objc_redeclaration_multi_2.swift -verify

@objc class Redecl1 {
  @objc init() { } // expected-note{{Objective-C method 'init' previously declared by initializer 'init()' here}}

  @objc
  func method1() { } // expected-note{{Objective-C method 'method1' previously declared by method 'method1()' here}}
}

extension Redecl2 {
  @objc(method1)
  func method1_alias() { } // expected-error{{method 'method1_alias()' redeclares Objective-C method 'method1'}}

  @objc(init)
  func initialize() { } // expected-error{{method 'initialize()' redeclares Objective-C method 'init'}}
}

extension Redecl1 {
  @objc(method2) func method2_alias() { } // expected-note{{Objective-C method 'method2' previously declared by method 'method2_alias()' here}}
}
