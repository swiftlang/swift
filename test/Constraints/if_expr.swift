// RUN: %swift -parse %s -verify

struct MyLogicValue : LogicValue {
  func getLogicValue() -> Bool {
    return true
  }
}

func useInt(x: Int) {}
func useDouble(x: Double) {}

class B {
  init() {} 
}
class D1 : B {
  init() { super.init() } 
}
class D2 : B {
  init() { super.init() } 
}

func useB(x: B) {}
func useD1(x: D1) {} // expected-note 4{{in initialization of parameter 'x'}}
func useD2(x: D2) {} // expected-note {{in initialization of parameter 'x'}}

var a = true ? 1 : 0 // should infer Int
var b : Double = true ? 1 : 0 // should infer Double
var c = true ? 1 : 0.0 // should infer Double
var d = true ? 1.0 : 0 // should infer Double

useInt(a)
useDouble(b)
useDouble(c)
useDouble(d)

var z = true ? a : b // expected-error{{}}

var e = true ? B() : B() // should infer B
var f = true ? B() : D1() // should infer B
var g = true ? D1() : B() // should infer B
var h = true ? D1() : D1() // should infer D1
var i = true ? D1() : D2() // should infer B

useB(e)
useD1(e) // expected-error{{}}
useB(f)
useD1(f) // expected-error{{}}
useB(g)
useD1(g) // expected-error{{}}
useB(h)
useD1(h)
useB(i)
useD1(i) // expected-error{{}}
useD2(i) // expected-error{{}}

var x = MyLogicValue() ? 1 : 0
var y = 22 ? 1 : 0 // expected-error{{}}
