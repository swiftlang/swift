// RUN: %target-typecheck-verify-swift -enable-experimental-feature ValueGenerics -enable-experimental-feature NonescapableTypes -disable-availability-checking -disable-experimental-parser-round-trip

protocol P {}

func invalid<let N>() {} // expected-error {{value generic 'N' must have an explicit value type declared}}
                         // expected-error@-1 {{generic parameter 'N' is not used in function signature}}
func invalid<let N>(_: A<N>) {} // expected-error {{value generic 'N' must have an explicit value type declared}}

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

  var n: N { // expected-error {{using value generic 'N' here is not allowed}}
    fatalError()
  }

  var nType: N.Type { // expected-error {{using value generic 'N' here is not allowed}}
    fatalError()
  }

  var a: A<N> { // OK
    fatalError()
  }
}

extension A where N: P {} // expected-error {{value generic type 'N' cannot conform to protocol 'P'}}

extension A where N == Int {} // expected-error {{cannot constrain value parameter 'N' to be type 'Int'}}

func b(with a: A<123>) {} // OK
func c<let M: Int>(with a: A<M>) {} // OK
func d<T>(with a: A<T>) {} // expected-error {{cannot pass type 'T' as a value for generic value 'N'}}
func e(with a: A<Int>) {} // expected-error {{cannot pass type 'Int' as a value for generic value 'N'}}

struct Generic<T: ~Copyable & ~Escapable> {}
struct GenericWithIntParam<T: ~Copyable & ~Escapable, let N: Int> {}

func f(_: Generic<123>) {} // expected-error {{integer unexpectedly used in a type position}}
func g<let N: Int>(_: Generic<N>) {} // expected-error {{cannot use value type 'N' for generic argument 'T'}}
func h(_: (Int, 123)) {} // expected-error {{integer unexpectedly used in a type position}}
func i(_: () -> 123) {} // expected-error {{integer unexpectedly used in a type position}}
func j(_: (A<123>) -> ()) {} // OK
func k(_: some 123) {} // expected-error {{integer unexpectedly used in a type position}}
func l(_: GenericWithIntParam<123, Int>) {} // expected-error {{cannot pass type 'Int' as a value for generic value 'N'}}
func m(_: GenericWithIntParam<Int, 123>) {} // OK

typealias One = 1 // expected-error {{integer unexpectedly used in a type position}}

struct B<let N: UInt8> {} // expected-error {{'UInt8' is not a supported value type for 'N'}}

struct C<let N: Int, let M: Int> {}

extension C where N == 123 { // expected-note {{where 'N' = '0'}}
                             // expected-note@-1 {{where 'N' = '0'}}
                             // expected-note@-2 {{where 'N' = 'T'}}
  func nIs123() {}
}

extension C where M == 321 { // expected-note {{where 'M' = '0'}}
                             // expected-note@-1 {{where 'M' = '0'}}
                             // expected-note@-2 {{where 'M' = 'T'}}
  func mIs123() {}
}

extension C where N == M { // expected-note {{where 'N' = '123', 'M' = '0'}}
                           // expected-note@-1 {{where 'N' = '0', 'M' = '321'}}
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
  c.nIs123() // expected-error {{referencing instance method 'nIs123()' on 'C' requires the types 'T' and '123' be equivalent}}
  c.mIs123() // expected-error {{referencing instance method 'mIs123()' on 'C' requires the types 'T' and '321' be equivalent}}
  c.nAndMAreBothEqual() // OK
}

struct D<let N: Int & P> {} // expected-error {{non-protocol, non-class type 'Int' cannot be used within a protocol-constrained type}}
