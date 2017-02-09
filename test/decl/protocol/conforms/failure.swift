// RUN: %target-typecheck-verify-swift

protocol P1 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // okay
}

extension P1 {
  func bar() {} // okay
}

struct P1Conformer : P1 {} // expected-error {{does not conform}}


protocol P2 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // expected-note {{protocol requires function 'bar()'}}
}
protocol P2Helper {}

extension P2 where Self : P2Helper {
  func bar() {} // expected-note {{candidate}}
}

struct P2Conformer : P2 {} // expected-error {{does not conform}}


protocol P3 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // okay
  func baz() -> Baz
  associatedtype Baz
}

extension P3 {
  func bar() {} // okay
}

struct P3Conformer : P3 { // expected-error {{does not conform}}
  func baz() -> Int { return 0 }
}


protocol P4 {
  func foo() // expected-note {{protocol requires function 'foo()'}}
  func bar() // expected-note {{protocol requires function 'bar()'}}
  func baz() -> Baz // okay
  associatedtype Baz
}
protocol P4Helper {}

extension P4 where Self : P4Helper {
  func bar() {} // expected-note {{candidate}}
}

struct P4Conformer : P4 { // expected-error {{does not conform}}
  func baz() -> Int { return 0 }
}


protocol P5 {
  associatedtype Foo
  func foo() -> Foo // expected-note {{protocol requires function 'foo()'}}
  func bar() -> Foo // okay
  func baz() -> Foo // okay
}

extension P5 {
  func bar() -> Foo { return foo() } // okay
}

struct P5Conformer : P5 { // expected-error {{does not conform}}
  func baz() -> Int { return 0 }
}


protocol P6Base {
  associatedtype Foo // expected-note {{protocol requires nested type 'Foo'; do you want to add it?}}
  func foo()
  func bar() -> Foo
}
extension P6Base {
}
protocol P6 : P6Base {
  associatedtype Bar // expected-note {{protocol requires nested type 'Bar'}}
}
extension P6 {
  func bar() -> Bar? { return nil }
}

struct P6Conformer : P6 { // expected-error 2 {{does not conform}}
  func foo() {}
}

// rdar://problem/23033862
// expected-error@+2{{type 'A' does not conform to protocol 'OptionSet'}}
// expected-error@+1{{type 'A' does not conform to protocol 'RawRepresentable'}}
struct A: OptionSet {
  let rawValue = 0
  init() { } // expected-note 2{{candidate has non-matching type '()'}}
}

// FIXME: Crappy diagnostic
protocol PA {
  associatedtype A // expected-note 2 {{protocol requires nested type 'A'; do you want to add it?}}
}

struct BadCase1 : PA { // expected-error {{type 'BadCase1' does not conform to protocol 'PA'}}
  struct A<T> {}
}

struct BadCase2 : PA { // expected-error {{type 'BadCase2' does not conform to protocol 'PA'}}
  typealias A<T> = T
}
