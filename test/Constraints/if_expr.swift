// RUN: %target-typecheck-verify-swift

func useInt(_ x: Int) {}
func useDouble(_ x: Double) {}

class B {
  init() {} 
}
class D1 : B {
  override init() { super.init() } 
}
class D2 : B {
  override init() { super.init() } 
}

func useB(_ x: B) {}
func useD1(_ x: D1) {}
func useD2(_ x: D2) {}

var a = true ? 1 : 0 // should infer Int
var b : Double = true ? 1 : 0 // should infer Double
var c = true ? 1 : 0.0 // should infer Double
var d = true ? 1.0 : 0 // should infer Double

useInt(a)
useDouble(b)
useDouble(c)
useDouble(d)

var z = true ? a : b // expected-error{{result values in '? :' expression have mismatching types 'Int' and 'Double'}}
var _ = a ? b : b // expected-error{{'Int' is not convertible to 'Bool'}}



var e = true ? B() : B() // should infer B
var f = true ? B() : D1() // should infer B
var g = true ? D1() : B() // should infer B
var h = true ? D1() : D1() // should infer D1
var i = true ? D1() : D2() // should infer B

useB(e)
useD1(e) // expected-error{{cannot convert value of type 'B' to expected argument type 'D1'}}
useB(f)
useD1(f) // expected-error{{cannot convert value of type 'B' to expected argument type 'D1'}}
useB(g)
useD1(g) // expected-error{{cannot convert value of type 'B' to expected argument type 'D1'}}
useB(h)
useD1(h)
useB(i)
useD1(i) // expected-error{{cannot convert value of type 'B' to expected argument type 'D1'}}
useD2(i) // expected-error{{cannot convert value of type 'B' to expected argument type 'D2'}}

var x = true ? 1 : 0
var y = 22 ? 1 : 0 // expected-error{{'Int' is not convertible to 'Bool'}}

_ = x ? x : x // expected-error {{'Int' is not convertible to 'Bool'}}
_ = true ? x : 1.2 // expected-error {{result values in '? :' expression have mismatching types 'Int' and 'Double'}}

_ = (x: true) ? true : false // expected-error {{'(x: Bool)' is not convertible to 'Bool'}}
_ = (x: 1) ? true : false // expected-error {{'(x: Int)' is not convertible to 'Bool'}}

let ib: Bool! = false
let eb: Bool? = .some(false)
let conditional = ib ? "Broken" : "Heart" // should infer Bool!
let conditional = eb ? "Broken" : "Heart" // expected-error {{value of optional type 'Bool?' not unwrapped; did you mean to use '!' or '?'?}}
