// RUN: %swift %s -verify

var t1 : Int
var t2 = 10
var t3 = 10, t4 = 20.0
var (t5, t6) = (10, 20.0)
var t7, t8 : Int
var t9, t10 = 20 // expected-error {{type annotation missing in pattern}}
var t11, t12 : Int = 20 // expected-error {{type annotation missing in pattern}}
var t13 = 2.0, t14 : Int
var (x : Int = 123, // expected-error {{default argument is only permitted for a non-curried function parameter}}
     y : Int = 456) // expected-error{{default argument is only permitted for a non-curried function parameter}}
@born_fragile
var bfx : Int, bfy : Int // no attribute error

var _ = 10

def _(x: Int) {} // expected-error {{expected identifier in function declaration}}


var self1 = self1 // expected-error {{variable used within its own initial value}}
var self2 : Int = self2 // expected-error {{variable used within its own initial value}}
var (self3 : Int) = self3 // expected-error {{variable used within its own initial value}}
var (self4) : Int = self4 // expected-error {{variable used within its own initial value}}
var self5 = self5 + self5 // expected-error 2 {{variable used within its own initial value}}
var self6 = !self6 // expected-error {{variable used within its own initial value}}
var (self7a : Int, self7b : Int) = (self7b, self7a) // expected-error 2 {{variable used within its own initial value}}

var self8 = 0
def testShadowing() {
  var self8 = self8 // expected-error {{variable used within its own initial value}}
}

var (paren) = 0
var paren2: Int = paren

struct Broken {
  var b : Bool = True // expected-error{{use of unresolved identifier 'True'}}
}
