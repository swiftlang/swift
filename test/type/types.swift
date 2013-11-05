// RUN: %swift %s -verify

var a : Int

def test() {
  var y : a   // expected-error {{use of undeclared type 'a'}} expected-note {{here}}
  var z : y   // expected-error {{'y' is not a type}}
}

var b : Int -> Int = {$0}

@auto_closure var c1 : () -> Int  // expected-error {{attribute can only be applied to types, not declarations}}
var c2 : (field : @auto_closure Int)  // expected-error {{attribute only applies to syntactic function types}}
var c3 : (field : @auto_closure Int -> Int)  // expected-error {{auto_closure argument type 'Int' must be '()'}}

var d1 : (field : @auto_closure () -> Int)
var d2 : @auto_closure () -> Int = 4

var d3 : @auto_closure () -> Float =
   4

var d4 : @auto_closure () -> Int =
   d2 // expected-error{{@auto_closure () -> Int' is not convertible to 'In}}
   
var e0 : Int[]
e0[] // expected-error {{expression does not type-check}}

var f0 : Float[]
var f1 : (Int,Int)[]

var g : swift // expected-error {{use of module 'swift' as a type}}

var h0 : Int?
!h0 // no-warning
var h1 : Int??
!h1! // no-warning
var h2 : Int?[]
var h3 : Int[]? // expected-error {{optional array type requires parentheses}} {{10-10=(}} {{15-15=)}}
var h4 : (Int[])?
var h5 : ((Int??[][])?[])?
var h6 : Int??[][]?[]? = h5 // expected-error {{optional array type requires parentheses}} {{10-10=(}} {{19-19=)}}  expected-error {{optional array type requires parentheses}} {{10-10=(}} {{22-22=)}}
var _ : Int = (h6![0]![0][0]!)! // no-warning
var h7 : (Int,Int)?
var h8 : (Int -> Int)?
var h9 : Int? -> Int?
var h10 : Int?.metatype?.metatype 

// We can't disambiguate this, but it won't matter when values auto-convert to Optional.
var i = Int?(42) // expected-error {{expression does not type-check}}

var j0 = new Int?[4]
var j1 = new Int.metatype[4] {i in Int}
var j2 = new (Int -> Int)[4] {i in {$0}}
var j3 = new (Int[])[4]
