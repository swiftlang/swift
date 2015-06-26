// RUN: %target-parse-verify-swift

struct MyLogicValue : BooleanType {
  var boolValue: Bool {
    return true
  }
}

func useInt(x: Int) {}
func useDouble(x: Double) {}

class B {
  init() {} 
}
class D1 : B {
  override init() { super.init() } 
}
class D2 : B {
  override init() { super.init() } 
}

func useB(x: B) {}
func useD1(x: D1) {}
func useD2(x: D2) {}

var a = true ? 1 : 0 // should infer Int
var b : Double = true ? 1 : 0 // should infer Double
var c = true ? 1 : 0.0 // should infer Double
var d = true ? 1.0 : 0 // should infer Double

useInt(a)
useDouble(b)
useDouble(c)
useDouble(d)

var z = true ? a : b // expected-error{{type of expression is ambiguous without more context}} 

var e = true ? B() : B() // should infer B
var f = true ? B() : D1() // should infer B
var g = true ? D1() : B() // should infer B
var h = true ? D1() : D1() // should infer D1
var i = true ? D1() : D2() // should infer B

useB(e)
useD1(e) // expected-error{{}} expected-note{{}}
useB(f)
useD1(f) // expected-error{{}} expected-note{{}}
useB(g)
useD1(g) // expected-error{{}} expected-note{{}}
useB(h)
useD1(h)
useB(i)
useD1(i) // expected-error{{}} expected-note{{}}
useD2(i) // expected-error{{}} expected-note{{}}

var x = MyLogicValue() ? 1 : 0
var y = 22 ? 1 : 0 // expected-error{{}}
