// RUN: %target-typecheck-verify-swift

class C<T> {}

protocol P {
  associatedtype A: C<B> // expected-note 2{{protocol requires nested type 'A'}}
  associatedtype B // expected-note 2{{protocol requires nested type 'B'}}

  func f() -> A
  func g() -> B
}

class D: C<Int> {}

struct S11: P {
  typealias A = C<Int>
  typealias B = Int

  func f() -> C<Int> { fatalError() }
  func g() -> Int { fatalError() }
}

struct S12: P {
  typealias A = D
  typealias B = Int

  func f() -> D { fatalError() }
  func g() -> Int { fatalError() }
}

struct S21: P {
  typealias A = C<Int>

  func f() -> C<Int> { fatalError() }
  func g() -> Int { fatalError() }
}

struct S22: P {
  typealias A = D

  func f() -> D { fatalError() }
  func g() -> Int { fatalError() }
}

struct S31: P {
  typealias B = Int

  func f() -> C<Int> { fatalError() }
  func g() -> Int { fatalError() }
}

struct S32: P {
  typealias B = Int

  func f() -> D { fatalError() }
  func g() -> Int { fatalError() }
}

struct S41: P {
  func f() -> C<Int> { fatalError() }
  func g() -> Int { fatalError() }
}

struct S42: P {
  func f() -> D { fatalError() }
  func g() -> Int { fatalError() }
}

struct SBad1: P { // expected-error {{type 'SBad1' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  func f() -> D { fatalError() }
  func g() -> String { fatalError() }
}

struct SBad2: P { // expected-error {{type 'SBad2' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  func f() -> C<Int> { fatalError() }
  func g() -> String { fatalError() }
}

protocol Q {
  associatedtype A: C<Self>

  func f() -> A
}

extension Q {
  func f() -> C<Self> { fatalError() }
}

struct QQ: Q {}