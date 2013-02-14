// RUN: %swift -parse -verify %s
struct A<B> {
  constructor(x:Int) {}
  static func c() {} // expected-note{{found this candidate}} expected-note{{found this candidate}} expected-note{{found this candidate}} expected-note{{found this candidate}} expected-note{{found this candidate}} expected-note{{found this candidate}} expected-note{{found this candidate}} expected-note{{found this candidate}} 
}
struct B {}

protocol Runcible {}
protocol Fungible {}

func meta<T>(m:T.metatype) {}
func meta2<T>(m:T.metatype, x:Int) {}

var a, b, c, d : Int

a < b
(a < b, c > d)
(a < b, c > (d))
// Parses as generic because of lparen_following after '>'
(a<b, c>(d)) // expected-error{{called expression isn't a function}}
a>(b)
a > (b)

// FIXME: Currently the type parameter list is dropped on the floor and these
// parse as just 'A' so none of these calls resolve.

A<B>.c() // expected-error{{no candidates found for call}}
A<A<B>>.c() // expected-error{{no candidates found for call}}
A<(A<B>) -> B>.c() // expected-error{{no candidates found for call}}
A<Int[][]>.c() // expected-error{{no candidates found for call}}
A<A<B>[][]>.c() // expected-error{{no candidates found for call}}
A<(Int, Char)>.c() // expected-error{{no candidates found for call}}
A<(a:Int, b:Char)>.c() // expected-error{{no candidates found for call}}
A<protocol<Runcible, Fungible>>.c() // expected-error{{no candidates found for call}}

A<B>(0) // expected-error{{inferred type 'A' is not compatible with literal}}

meta(A<B>)

meta2(A<B>, 0)

