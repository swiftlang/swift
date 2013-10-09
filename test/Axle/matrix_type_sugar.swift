// RUN: %swift -std=axle -parse %s -verify

// FIXME: When the matrix structs actually get implemented, these
// errors will go away.

// Parsing in a type context
var m2f : Matrix<Float, 2> // expected-error{{missing matrix implementation type Matrix2f for 'Matrix<Float, 2>'}}
var m2x2f : Matrix<Float, 2, 2> // expected-error{{missing matrix implementation type Matrix2f for 'Matrix<Float, 2, 2>'}}
var m2x3d : Matrix<Double, 2, 3> // expected-error{{missing matrix implementation type Matrix2x3d for 'Matrix<Double, 2, 3>'}}

// Parsing in an expression context
var v4f = Matrix<Float, 2>() // expected-error{{missing matrix implementation type Matrix2f for 'Matrix<Float, 2>'}} \
 // expected-error{{expression does not type-check}}

// Bad element type
struct X { }
var m2X : Matrix<X, 2>; // expected-error{{invalid matrix element type 'X'}}

// Missing desugared struct kind
var m17f : Matrix<Float, 17> // expected-error{{missing matrix implementation type Matrix17f for 'Matrix<Float, 17>}}


