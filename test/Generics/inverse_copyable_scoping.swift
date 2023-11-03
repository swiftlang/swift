// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

protocol NoCopyReq: ~Copyable {}

protocol P {
  func f() where Self: ~Copyable // expected-error {{cannot add inverse constraint 'Self: ~Copyable' on generic parameter 'Self' defined in outer scope}}

  func g<T>(_: T) where Self: ~Copyable // expected-error {{cannot add inverse constraint 'Self: ~Copyable' on generic parameter 'Self' defined in outer scope}}

  associatedtype AT where Self: ~Copyable // expected-error {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}

  // expected-error@+1 {{cannot add inverse constraint 'Self.Alice: ~Copyable' on generic parameter 'Self.Alice' defined in outer scope}}
  associatedtype Bob where Alice: NoCopyReq & ~Copyable
  associatedtype Alice where Bob: ~Copyable
  // expected-error@-1 {{cannot add inverse constraint 'Self.Bob: ~Copyable' on generic parameter 'Self.Bob' defined in outer scope}}
}

protocol U {}

extension U where Self: ~Copyable {} // expected-error {{cannot add inverse constraint 'Self: ~Copyable' on generic parameter 'Self' defined in outer scope}}

extension P {
  func g() where Self: ~Copyable {} // expected-error {{cannot add inverse constraint 'Self: ~Copyable' on generic parameter 'Self' defined in outer scope}}

  typealias Me = Self where Self: ~Copyable // expected-error {{cannot add inverse constraint 'Self: ~Copyable' on generic parameter 'Self' defined in outer scope}}

  typealias MeAndU = Self where Self: U
}

struct S<T> {

  // expected-note@+2 3{{add}}
  // expected-error@+1 {{parameter of noncopyable type 'U' must specify ownership}}
  func fn<U>(_ u: U)
    where T: ~Copyable, // expected-error {{cannot add inverse constraint 'T: ~Copyable' on generic parameter 'T' defined in outer scope}}
          U: ~Copyable
          {}

  func onlyCopyable() where T: Copyable {}
}

extension S where T: NoCopyReq & ~Copyable {} // expected-error {{cannot add inverse constraint 'T: ~Copyable' on generic parameter 'T' defined in outer scope}}
