// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/OtherOS.swiftmodule -module-name OtherOS %S/Inputs/witness_fix_its_other_module.swift -parse-as-library
// RUN: %target-typecheck-verify-swift -I %t/

prefix operator ^^^
postfix operator ^^^^

import OtherOS

protocol Foo {
  var bar1: Int { get set } // expected-note {{protocol requires property 'bar1' with type 'Int'}}
  static var bar2: Int { get set } // expected-note {{protocol requires property 'bar2' with type 'Int'}}
  var bar3: Int { get set } // expected-note {{protocol requires property 'bar3' with type 'Int'}}
  static prefix func ^^^(value: Self) -> Int // expected-note {{protocol requires function '^^^' with type '(ConformsToFoo) -> Int'}}
  static postfix func ^^^^(value: Self) -> Int // expected-note {{protocol requires function '^^^^' with type '(ConformsToFoo) -> Int'}}
  func bar4(closure: () throws -> Int) rethrows // expected-note {{protocol requires function 'bar4(closure:)' with type '(() throws -> Int) throws -> ()'}}
  var bar5: Int { get set } // expected-note {{protocol requires property 'bar5' with type 'Int'}}
  static subscript(_ pos: Int) -> Int { get } // expected-note {{protocol requires subscript with type '(Int) -> Int'}}
}

struct ConformsToFoo: Foo { // expected-error {{type 'ConformsToFoo' does not conform to protocol 'Foo'}} expected-note {{add stubs for conformance}}
  let bar1: Int // expected-note {{candidate is not settable, but protocol requires it}}{{3-6=var}}
  var bar2: Int // expected-note {{candidate operates on an instance, not a type as required}}{{3-3=static }}
  static var bar3: Int = 1 // expected-note {{candidate operates on a type, not an instance as required}}{{3-10=}}
  static postfix func ^^^(value: ConformsToFoo) -> Int { return 0 } // expected-error {{operator implementation without matching operator declaration}}
  // expected-note@-1 {{candidate is postfix, not prefix as required}}{{10-17=prefix}}
  static prefix func ^^^^(value: ConformsToFoo) -> Int { return 0 } // expected-error {{operator implementation without matching operator declaration}}
  // expected-note@-1 {{candidate is prefix, not postfix as required}}{{10-16=postfix}}
  func bar4(closure: () throws -> Int) throws {} // expected-note {{candidate is not 'rethrows', but protocol requires it}}{{40-46=rethrows}}
  var bar5: Int { return 0 } // expected-note {{candidate is not settable, but protocol requires it}}{{none}}
  subscript(_ pos: Int) -> Int { return 0 } // expected-note {{candidate operates on an instance, not a type as required}}{{3-3=static }}
}

protocol Foo1 {
  subscript(value: Bool) -> Bool { get set } // expected-note {{protocol requires subscript with type '(Bool) -> Bool'}}
}

struct ConformsToFoo1: Foo1 { // expected-error {{type 'ConformsToFoo1' does not conform to protocol 'Foo1'}} expected-note {{add stubs for conformance}}
  subscript(value: Bool) -> Bool { return false } // expected-note {{candidate is not settable, but protocol requires it}}{{none}}
}

// Don't crash or try to fixup get/set mismatches in other modules
// This protocol requirement must conflict with the one in
// witness_fix_its_other_module.swift.
protocol RenameableProtocol {
  var name: String { get set } // expected-note {{protocol requires property 'name' with type 'String'}}
}

extension Linux: RenameableProtocol {} 
// expected-error@-1 {{type 'Linux' does not conform to protocol 'RenameableProtocol'}}
// expected-note@-2 {{add stubs for conformance}}
