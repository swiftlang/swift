// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonescapableTypes

protocol P {}

struct A<let N: Int> {
  var int: Int {
    N // OK
  }

  var uint: UInt {
    N // expected-error {{cannot convert return expression of type 'Int' to return type 'UInt'}}
  }

  var goodUInt: UInt {
    UInt(N) // OK
  }

  var intSelf: Int {
    N.self // OK
  }
}

extension A where N: P {} // expected-error {{value generic type 'let N' cannot conform to protocol 'P'}}

extension A where N == Int {} // expected-error {{cannot constrain value parameter 'let N' to be type 'Int'}}

func b(with a: A<123>) {} // OK
func c<let M: Int>(with a: A<M>) {} // OK
func d<T>(with a: A<T>) {} // expected-error {{cannot pass type 'T' as a value for generic value 'let N'}}
func e(with a: A<Int>) {} // expected-error {{cannot pass type 'Int' as a value for generic value 'let N'}}

struct Generic<T: ~Copyable & ~Escapable> {}
struct GenericWithIntParam<T: ~Copyable & ~Escapable, let N: Int> {}

func f(_: Generic<123>) {} // expected-error {{integer unexpectedly used in a type position}}
func g<let N: Int>(_: Generic<N>) {} // expected-error {{cannot use value type 'let N' for generic argument 'T'}}
func h(_: (Int, 123)) {} // expected-error {{integer unexpectedly used in a type position}}
func i(_: () -> 123) {} // expected-error {{integer unexpectedly used in a type position}}
func j(_: (A<123>) -> ()) {} // OK
func k(_: some 123) {} // expected-error {{integer unexpectedly used in a type position}}
func l(_: GenericWithIntParam<123, Int>) {} // expected-error {{cannot pass type 'Int' as a value for generic value 'let N'}}
func m(_: GenericWithIntParam<Int, 123>) {} // OK

typealias One = 1 // expected-error {{integer unexpectedly used in a type position}}

struct B<let N: UInt8> {} // expected-error {{'UInt8' is not a supported value type for 'let N'}}

struct C<let N: Int, let M: Int> {}

extension C where N == 123 { // expected-note {{where 'let N' = '0'}}
                             // expected-note@-1 {{where 'let N' = '0'}}
                             // expected-note@-2 {{where 'let N' = 'let T'}}
  func nIs123() {}
}

extension C where M == 321 { // expected-note {{where 'let M' = '0'}}
                             // expected-note@-1 {{where 'let M' = '0'}}
                             // expected-note@-2 {{where 'let M' = 'let T'}}
  func mIs123() {}
}

extension C where N == M { // expected-note {{where 'let N' = '123', 'let M' = '0'}}
                           // expected-note@-1 {{where 'let N' = '0', 'let M' = '321'}}
  func nAndMAreBothEqual() {}
}

func testC1(with c: C<0, 0>) {
  c.nIs123() // expected-error {{referencing instance method 'nIs123()' on 'C' requires the types '0' and '123' be equivalent}}
  c.mIs123() // expected-error {{referencing instance method 'mIs123()' on 'C' requires the types '0' and '321' be equivalent}}
  c.nAndMAreBothEqual() // OK
}

func testC2(with c: C<123, 0>) {
  c.nIs123() // OK
  c.mIs123() // expected-error {{referencing instance method 'mIs123()' on 'C' requires the types '0' and '321' be equivalent}}
  c.nAndMAreBothEqual() // expected-error {{referencing instance method 'nAndMAreBothEqual()' on 'C' requires the types '123' and '0' be equivalent}}
}

func testC3(with c: C<0, 321>) {
  c.nIs123() // expected-error {{referencing instance method 'nIs123()' on 'C' requires the types '0' and '123' be equivalent}}
  c.mIs123() // OK
  c.nAndMAreBothEqual() // expected-error {{referencing instance method 'nAndMAreBothEqual()' on 'C' requires the types '0' and '321' be equivalent}}
}

func testC4<let T: Int>(with c: C<T, T>) {
  c.nIs123() // expected-error {{referencing instance method 'nIs123()' on 'C' requires the types 'let T' and '123' be equivalent}}
  c.mIs123() // expected-error {{referencing instance method 'mIs123()' on 'C' requires the types 'let T' and '321' be equivalent}}
  c.nAndMAreBothEqual() // OK
}

struct D<let N: Int & P> {} // expected-error {{non-protocol, non-class type 'Int' cannot be used within a protocol-constrained type}}
