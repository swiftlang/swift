// RUN: %target-parse-verify-swift
//
// <rdar://problem/19422987> test/Parse/self_keyword_recovery.swift is XFAIL'ed and it regressed
// XFAIL: *
//
// Test recovery for 'self' and 'Self' keywords used in inappropriate places.

//===--- Helper types used in tests below.

protocol FooProtocol {}

//===--- Tests.

func self() {} // expected-error {{expected identifier in function declaration}}
func Self() {} // expected-error {{expected identifier in function declaration}}

func super {} // expected-error {{expected identifier in function declaration}} expected-error {{expected '(' in argument list of function declaration}}
func weak -> Int {} // expected-error {{expected identifier in function declaration}} expected-error {{expected '(' in argument list of function declaration}}

func freeFunc1() -> self {} // expected-error {{expected type}}
func freeFunc2() -> Self {} // expected-error {{global function cannot return 'Self'}}

func freeFunc3(self: Int) {} // expected-error {{keyword 'self' cannot be used as an identifier}}
func freeFunc4(Self: Int) {} // expected-error {{keyword 'Self' cannot be used as an identifier}}

func freeFunc5(a: self) {} // expected-error {{expected type}}
func freeFunc6(a: Self) {} // expected-error {{use of undeclared type 'Self'}}

struct Structs {
  struct self {}                  // expected-error {{expected identifier in struct declaration}}
  struct self : FooProtocol {}    // expected-error {{expected identifier in struct declaration}}
  struct self<T> {}               // expected-error {{expected identifier in struct declaration}}
  struct self<T> : FooProtocol {} // expected-error {{expected identifier in struct declaration}}

  struct Self {}                  // expected-error {{expected identifier in struct declaration}}
}

struct Enums {
  enum self {} // expected-error {{expected identifier in enum declaration}}
  enum Self {} // expected-error {{expected identifier in enum declaration}}
}

struct Classes {
  class self {}                  // expected-error {{expected identifier in class declaration}}
  class self : FooProtocol {}    // expected-error {{expected identifier in class declaration}}
  class self<T> {}               // expected-error {{expected identifier in class declaration}}
  class self<T> : FooProtocol {} // expected-error {{expected identifier in class declaration}}

  class Self {} // expected-error {{expected identifier in class declaration}}

  extension self {} // expected-error {{expected type}} expected-error {{declaration is only valid at file scope}}
  // FIXME: this errors out for the wrong reason
  extension Self {} // expected-error {{declaration is only valid at file scope}}

  extension super {} // expected-error {{expected type}} expected-error {{declaration is only valid at file scope}}
  extension func {} // expected-error {{expected type}} expected-error {{declaration is only valid at file scope}}
}

protocol SelfExtensionA {
  extension self {} // expected-error {{expected type}} expected-error {{declaration is only valid at file scope}}
}

protocol SelfExtensionB {
  // FIXME: this errors out for the wrong reason
  extension Self {} // expected-error {{declaration is only valid at file scope}}
}

// FIXME: this errors out for the wrong reason
extension Self {} // expected-error {{use of undeclared type 'Self'}}

struct Protocols {
  protocol self {}               // expected-error {{expected identifier in protocol declaration}} expected-error {{declaration is only valid at file scope}}
  protocol self : FooProtocol {} // expected-error {{expected identifier in protocol declaration}} expected-error {{declaration is only valid at file scope}}

  protocol Self {} // expected-error {{expected identifier in protocol declaration}} expected-error {{declaration is only valid at file scope}}
}

struct Typealiases {
  typealias self = Int               // expected-error {{expected identifier in typealias declaration}}

  typealias Self = Int               // expected-error {{expected identifier in typealias declaration}}
}

enum FooEnum1 {
  case self // expected-error {{expected identifier in enum case declaration}}
  case Self // expected-error {{expected identifier in enum case declaration}}
}

enum FooEnum2 {
  case self: // expected-error {{'case' label can only appear inside a 'switch' statement}}
  case Self: // expected-error {{'case' label can only appear inside a 'switch' statement}}
}

enum FooEnum3 {
  case self(Int) // expected-error {{expected identifier in enum case declaration}}
  case Self(Int) // expected-error {{expected identifier in enum case declaration}}
}

enum FooEnum4 {
  case self(Int): // expected-error {{expected identifier in enum case declaration}} expected-error {{'case' label can only appear inside a 'switch' statement}}
  case Self(Int): // expected-error {{expected identifier in enum case declaration}} expected-error {{'case' label can only appear inside a 'switch' statement}}
}

