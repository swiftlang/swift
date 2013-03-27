// RUN: %swift -parse -verify %s

class B { }
class D : B { }

var seven = 7 as Double

var pair = (1, 2) as (Int, Double)

var closure = { $0 + $1 } as (Int, Int) -> Int

var d_as_b = new D as B
var b_as_d = new B as! D
var bad_b_as_d = new B as D // expected-error{{}}

func archetype_casts<T:B>(t:T) {
  var t_as_b = t as B
  var b_as_t = new B as! T
  var bad_b_as_t = new B as T // expected-error{{}}
}
