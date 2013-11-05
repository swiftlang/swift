// RUN: %swift %s -verify

var t1: (Int...) = ()
var t1: (Int, Int...) = () // expected-error {{expression does not type-check}}

var t2: (Int...) = 1
var t2: (Int, Int ...) = 1
var t2: (Int, Int, Int...) = 1  // expected-error {{expression does not type-check}}
var t2: (Double = 0.0, Int...) = 1 // expected-error {{default argument not permitted in a tuple type}}

var t3: (Int...) = (1,2)
var t3: (Int, Int...) = (1,2)
var t3: (Int, Int, Int...) = (1,2)
var t3: (Int, Int, Int, Int...) = (1,2) // expected-error {{different number of elements}}

var t4: (Int...) -> Int
var t5: (a: Int...) -> Int = t4
var t6: (a: Int[]) -> Int = t4 // expected-error {{does not type-check}}

var t7: Int
var t8: (Int, String...) = (t7)

def f1(a: Int...) { for x in a {} }
f1()
f1(1)
f1(1,2)
def f2(a: Int, b: Int...) { for x in b {} }
f2(1)
f2(1,2)
f2(1,2,3)

def f3(a: (String) -> Void) { }
f3({ printf("%s\n", $0) })


def f4(a: Int..., b: Int) { } // expected-error{{variadic arguments '...' must come at the end of the pattern}}

def f5(a: Int, (b, c): (Int, Int)...) {} // expected-error {{tuple pattern cannot match values of the non-tuple type '(Int, Int)[]'}}
