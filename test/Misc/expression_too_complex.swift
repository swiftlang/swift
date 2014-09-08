// RUN: %swift -parse %s -verify -solver-memory-threshold 100000

var x = [1, 2, 3, [4, 5], 6] // expected-error{{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}

// No errors should appear below as a result of the error above.
var y = 10
var z = 10 + 10

class C {}

var c = C()
var d = c