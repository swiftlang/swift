// RUN: %target-parse-verify-swift

var t1a: (Int...) = (1) // expected-error{{cannot create a variadic tuple}}
var t2d: (Double = 0.0) = 1 // expected-error {{default argument not permitted in a tuple type}}

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
