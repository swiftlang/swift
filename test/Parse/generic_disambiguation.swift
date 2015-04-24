// RUN: %target-parse-verify-swift

struct A<B> { // expected-note{{generic type 'A' declared here}}
  init(x:Int) {}
  static func c() {}

  struct C<D> { // expected-error{{generic type 'C' nested in type}}
    static func e() {}
  }

  struct F {} // expected-error{{nested in generic type}}
}
struct B {}
struct D {}

protocol Runcible {}
protocol Fungible {}

func meta<T>(m: T.Type) {}
func meta2<T>(m: T.Type, _ x: Int) {}

func generic<T>(x: T) {}

var a, b, c, d : Int

a < b
(a < b, c > d)
// Parses as generic because of lparen after '>'
(a < b, c > (d)) // expected-error{{use of undeclared type 'b'}} expected-note{{while parsing this '<' as a type parameter bracket}}
// Parses as generic because of lparen after '>'
(a<b, c>(d)) // expected-error{{use of undeclared type 'b'}} expected-note{{while parsing this '<' as a type parameter bracket}}
a>(b)
a > (b)

generic<Int>(0) // expected-error{{cannot explicitly specialize a generic function}} expected-note{{while parsing this '<' as a type parameter bracket}}

A<B>.c()
A<A<B>>.c()
A<A<B>.F>.c()
A<(A<B>) -> B>.c()
A<[[Int]]>.c()
A<[[A<B>]]>.c()
A<(Int, UnicodeScalar)>.c()
A<(a:Int, b:UnicodeScalar)>.c()
A<protocol<Runcible, Fungible>>.c()

A<B>(x: 0)

meta(A<B>.self)

meta2(A<B>.self, 0)

// FIXME: Nested generic types. Need to be able to express $T0<A, B, C> in the
// typechecker.
/*
A<B>.C<D>.e()

A<B>.C<D>(0)

meta(A<B>.C<D>.self)
meta2(A<B>.C<D>.self, 0)
 */

// TODO: parse empty <> list
//A<>.c() // e/xpected-error{{xxx}}

A<B, D>.c() // expected-error{{generic type 'A' specialized with too many type parameters (got 2, but expected 1)}} expected-error {{'<<error type>>.Type' does not have a member named 'c'}}

A<B?>(x: 0) // parses as type
a < b ? c : d


