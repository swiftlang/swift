// RUN: %swift %s -verify

func f0(_ : Int, _ : Float) { }

var i : Int
var d : Double

// Tuple element.
f0(i,
   d // expected-error{{invalid conversion from type 'Double' to 'Float'}}
   )
