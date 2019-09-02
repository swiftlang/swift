// RUN: %target-typecheck-verify-swift

prefix operator ^^^
postfix operator ^^^^

protocol Foo {
  var bar1: Int { get set } // expected-note {{protocol requires property 'bar1' with type 'Int'; do you want to add a stub?}}
  static var bar2: Int { get set } // expected-note {{protocol requires property 'bar2' with type 'Int'; do you want to add a stub?}}
  var bar3: Int { get set } // expected-note {{protocol requires property 'bar3' with type 'Int'; do you want to add a stub?}}
  static prefix func ^^^(value: Self) -> Int // expected-note {{protocol requires function '^^^' with type '(ConformsToFoo) -> Int'; do you want to add a stub?}}
  static postfix func ^^^^(value: Self) -> Int // expected-note {{protocol requires function '^^^^' with type '(ConformsToFoo) -> Int'; do you want to add a stub?}}
  func bar4() // expected-note {{protocol requires function 'bar4()' with type '() -> ()'; do you want to add a stub?}}
  func bar5(closure: () throws -> Int) rethrows // expected-note {{protocol requires function 'bar5(closure:)' with type '(() throws -> Int) throws -> ()'; do you want to add a stub?}}
  func bar6() // expected-note {{protocol requires function 'bar6()' with type '() -> ()'; do you want to add a stub?}}
}

struct ConformsToFoo: Foo { // expected-error {{type 'ConformsToFoo' does not conform to protocol 'Foo'}}
  let bar1: Int // expected-note {{candidate is not settable, but protocol requires it}}{{3-6=var}}
  var bar2: Int // expected-note {{candidate operates on an instance, not a type as required}}{{3-3=static}}
  static var bar3: Int = 1 // expected-note {{candidate operates on a type, not an instance as required}}{{3-10=}}
  static postfix func ^^^(value: ConformsToFoo) -> Int { return 0 } // expected-error {{operator implementation without matching operator declaration}}
  // expected-note@-1 {{candidate is postfix, not prefix as required}}{{10-17=prefix}}
  static prefix func ^^^^(value: ConformsToFoo) -> Int { return 0 } // expected-error {{operator implementation without matching operator declaration}}
  // expected-note@-1 {{candidate is prefix, not postfix as required}}{{10-16=postfix}}
  mutating func bar4() {} // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}{{3-12=}}
  func bar5(closure: () throws -> Int) throws {} // expected-note {{candidate is not 'rethrows', but protocol requires it}}{{40-46=rethrows}}
  func bar6() throws {} // expected-note {{candidate throws, but protocol does not allow it}}{{15-22=}}
}
