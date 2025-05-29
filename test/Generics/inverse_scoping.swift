// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypes

// REQUIRES: swift_feature_SuppressedAssociatedTypes



protocol NoCopyReq: ~Copyable {}

protocol P {
  func f() where Self: ~Copyable // expected-error {{cannot suppress '~Copyable' on generic parameter 'Self' defined in outer scope}}

  func g<T>(_: T) where Self: ~Copyable // expected-error {{cannot suppress '~Copyable' on generic parameter 'Self' defined in outer scope}}

  associatedtype AT where Self: ~Copyable // expected-error {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}

  // expected-error@+1 {{cannot suppress '~Copyable' on generic parameter 'Self.Alice' defined in outer scope}}
  associatedtype Bob where Alice: NoCopyReq & ~Copyable
  associatedtype Alice where Bob: ~Copyable
  // expected-error@-1 {{cannot suppress '~Copyable' on generic parameter 'Self.Bob' defined in outer scope}}
}

protocol U {}

extension U where Self: ~Copyable {}
// expected-error@-1 {{'Self' required to be 'Copyable' but is marked with '~Copyable'}}

extension P where Self: ~Copyable {
  func g() where Self: ~Copyable, // expected-error {{cannot suppress '~Copyable' on generic parameter 'Self' defined in outer scope}}
                                  // FIXME: why no similar 2nd error as Escapable here on Self?

                 Self: ~Escapable {}  // expected-error {{cannot suppress '~Escapable' on generic parameter 'Self' defined in outer scope}}
                                      // expected-error@-1 {{'Self' required to be 'Escapable' but is marked with '~Escapable'}}

  typealias Me = Self where Self: ~Copyable // expected-error {{cannot suppress '~Copyable' on generic parameter 'Self' defined in outer scope}}

  typealias MeAndU = Self where Self: U
}

struct S<T> {

  // expected-note@+2 3{{add}}
  // expected-error@+1 {{parameter of noncopyable type 'U' must specify ownership}}
  func fn<U>(_ u: U)
    where T: ~Copyable, // expected-error {{cannot suppress '~Copyable' on generic parameter 'T' defined in outer scope}}
                        // expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}
          U: ~Copyable
          {}

  func onlyCopyable() where T: Copyable {}

    func fn<U>(_ u: U)
      where T: ~Escapable, // expected-error {{cannot suppress '~Escapable' on generic parameter 'T' defined in outer scope}}
                           // expected-error@-1 {{'T' required to be 'Escapable' but is marked with '~Escapable'}}
            U: ~Escapable
            {}
}

extension S where T: NoCopyReq & ~Copyable {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

struct ExtraInverse<T: ~Copyable> {
  func check() where T: ~Copyable {} // expected-error {{cannot suppress '~Copyable' on generic parameter 'T' defined in outer scope}}
}
