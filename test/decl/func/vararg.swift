// RUN: %target-parse-verify-swift

var t1a: (Int...) = (1) // expected-error{{cannot create a variadic tuple}}
var t2d: (Double = 0.0) = 1 // expected-error {{default argument not permitted in a tuple type}} {{18-23=}}

func f1(a: Int...) { for _ in a {} }
f1()
f1(1)
f1(1,2)
func f2(a: Int, _ b: Int...) { for _ in b {} }
f2(1)
f2(1,2)
f2(1,2,3)

func f3(a: (String) -> Void) { }
f3({ print($0) })


func f4(a: Int..., b: Int) { }

// rdar://16008564
func inoutVariadic(i: inout Int...) {  // expected-error {{inout arguments cannot be variadic}}
}

// rdar://19722429
func invalidVariadic(e: NonExistentType) { // expected-error {{use of undeclared type 'NonExistentType'}}
  { (e: ExtraCrispy...) in }() // expected-error {{use of undeclared type 'ExtraCrispy'}}
}

func twoVariadics(a: Int..., b: Int...) { } // expected-error{{only a single variadic parameter '...' is permitted}} {{36-39=}}

// rdar://22056861
func f5(list: Any..., end: String = "") {}
f5(String())

// rdar://18083599
enum E1 {
  case On, Off
}

func doEV(state: E1...) {}
doEV(.On)
