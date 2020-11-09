// RUN: %target-typecheck-verify-swift

var t1a: (Int...) = (1) // expected-error{{cannot create a variadic tuple}}
var t2d: (Double = 0.0) = 1 // expected-error {{default argument not permitted in a tuple type}} {{18-23=}}

func f1(_ a: Int...) { for _ in a {} }
f1()
f1(1)
f1(1,2)
func f2(_ a: Int, _ b: Int...) { for _ in b {} }
f2(1)
f2(1,2)
f2(1,2,3)

func f3(_ a: (String) -> Void) { }
f3({ print($0) })


func f4(_ a: Int..., b: Int) { }

// rdar://16008564
func inoutVariadic(_ i: inout Int...) {  // expected-error {{'inout' must not be used on variadic parameters}}
}

// rdar://19722429
func invalidVariadic(_ e: NonExistentType) { // expected-error {{cannot find type 'NonExistentType' in scope}}
  { (e: ExtraCrispy...) in }() // expected-error {{cannot find type 'ExtraCrispy' in scope}}
}

func twoVariadics(_ a: Int..., b: Int...) { }
func unlabeledFollowingVariadic(_ a: Int..., _ b: Int) { } // expected-error {{a parameter following a variadic parameter requires a label}}
func unlabeledVariadicFollowingVariadic(_ a: Int..., _ b: Int...) { } // expected-error {{a parameter following a variadic parameter requires a label}}
func unlabeledFollowingTwoVariadics(_ a: Int..., b: Int..., _ c: Int) { } // expected-error {{a parameter following a variadic parameter requires a label}}
func splitVariadics(_ a: Int..., b: Int, _ c: String...) { }
func splitByDefaultArgVariadics(_ a: Int..., b: Int = 0, _ c: String...) { }

struct HasSubscripts {
  subscript(a: Int...) -> Void { () }
  subscript(a: Int..., b b: Int...) -> Void { () }
  subscript(a: Int..., b: Int...) -> Void { () } // expected-error {{a parameter following a variadic parameter requires a label}}
  subscript(a: Int..., b: Int) -> Void { () } // expected-error {{a parameter following a variadic parameter requires a label}}
  subscript(a: Int..., b b: Int..., c c: Int) -> Void { () }
  subscript(a: Int..., b b: Int..., c: Int) -> Void { () } // expected-error {{a parameter following a variadic parameter requires a label}}
  subscript(a: Int..., c c: Int = 0, b: Int...) -> Void { () }
  subscript(a: Int..., b: String = "hello, world!") -> Bool { false } // expected-error {{a parameter following a variadic parameter requires a label}}
}

struct HasInitializers {
  init(a: Int...) {}
  init(a: Int..., b: Int...) {}
  init(a: Int..., _ b: Int...) {} // expected-error {{a parameter following a variadic parameter requires a label}}
  init(a: Int..., c: Int = 0, _ b: Int...) {}
}

let closure = {(x: Int..., y: Int...) in } // expected-error {{no parameters may follow a variadic parameter in a closure}}
let closure2 = {(x: Int..., y: Int) in } // expected-error {{no parameters may follow a variadic parameter in a closure}}
let closure3 = {(x: Int..., y: Int, z: Int...) in } // expected-error {{no parameters may follow a variadic parameter in a closure}}
let closure4 = {(x: Int...) in }
let closure5 = {(x: Int, y: Int...) in }
let closure6 = {(x: Int..., y z: Int) in } // expected-error {{closure cannot have keyword arguments}}
// expected-error@-1 {{no parameters may follow a variadic parameter in a closure}}

// rdar://22056861
func f5(_ list: Any..., end: String = "") {}
f5(String())

// rdar://18083599
enum E1 {
  case On, Off
}

func doEV(_ state: E1...) {}
doEV(.On)
