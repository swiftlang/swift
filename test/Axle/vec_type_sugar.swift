// RUN: %swift -std=axle -parse %s -verify

// Parsing in a type context
var v2f : Vec<Float, 2> = Vec2f(1.5, 2.5)
var v2b : Vec<Bool, 2> = Vec2b(false, true)
var v2i8 : Vec<Int8, 2> = Vec2i8(1, 5)

// Parsing in an expression context
var v4f = Vec<Float, 4>(1.5, 2.5, -1.5, -2.5)
v4f = Vec< Float, 4 > (1.5, 2.5, -1.5, -2.5)

// Bad element type
struct X { }
var v2X : Vec<X, 2>; // expected-error{{invalid vector element type 'X'}}

// Missing desugared struct kind
var v17f : Vec<Float, 17> // expected-error{{missing vector implementation type Vec17f for 'Vec<Float, 17>}}


