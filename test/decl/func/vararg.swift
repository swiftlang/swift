// RUN: %swift %s -verify

var t1a: (Int...) = ()
var t1b: (Int, Int...) = () // expected-error {{cannot convert the expression's type '()' to type '(Int, Int...)'}}

var t2a: (Int...) = 1
var t2b: (Int, Int ...) = 1
var t2c: (Int, Int, Int...) = 1  // expected-error {{cannot convert the expression's type 'Int' to type '(Int, Int, Int...)'}}
var t2d: (Double = 0.0, Int...) = 1 // expected-error {{default argument not permitted in a tuple type}}

var t3a: (Int...) = (1,2)
var t3b: (Int, Int...) = (1,2)
var t3c: (Int, Int, Int...) = (1,2)
var t3d: (Int, Int, Int, Int...) = (1,2) // expected-error {{different number of elements}}

var t4: (Int...) -> Int
var t5: (a: Int...) -> Int = t4
var t6: (a: Int[]) -> Int = t4 // expected-error {{cannot convert the expression's type '@lvalue (Int...) -> Int' to type '(a: Int[]) -> Int'}}

var t7: Int
var t8: (Int, String...) = (t7)

func f1(a: Int...) { for x in a {} }
f1()
f1(1)
f1(1,2)
func f2(a: Int, b: Int...) { for x in b {} }
f2(1)
f2(1,2)
f2(1,2,3)

func f3(a: (String) -> Void) { }
f3({ print($0) })


func f4(a: Int..., b: Int) { } // expected-error{{'...' must be on the last parameter}}

// rdar://16008564
func inout_variadic(inout i: Int...) {  // expected-error {{inout arguments cannot be variadic}}
}
