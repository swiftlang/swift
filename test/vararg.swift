// RUN: %swift %s -verify

var t1 : (Int...) = ()
var t1 : (Int, Int...) = () // expected-error {{no value to initialize tuple element #0 in expression of type '()'}} expected-note {{while converting}}

var t2 : (Int...) = 1
var t2 : (Int, Int ...) = 1
var t2 : (Int, Int, Int...) = 1  // expected-error {{inferred type '(Int, Int, Int...)' is not compatible with literal}} expected-note {{while converting}}
var t2 : (Double = 0.0, Int...) = 1 // expected-error {{invalid conversion from type 'Int' to 'Double'}}  expected-note {{while converting}}

var t3 : (Int...) = (1,2)
var t3 : (Int, Int...) = (1,2)
var t3 : (Int, Int, Int...) = (1,2)
var t3 : (Int, Int, Int, Int...) = (1,2) // expected-error {{no value to initialize tuple element #2 in expression of type '(<<dependent type>>, <<dependent type>>)'}} expected-note {{while converting}}
