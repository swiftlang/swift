// RUN: %swift %s -verify

struct MyLogicValue {
  func getLogicValue() -> Bool {
    return true
  }
}

func useInt(x:Int) {}
func useDouble(x:Double) {}

class B {}
class D1 : B {}
class D2 : B {}

func useB(x:B) {}
func useD1(x:D1) {}
func useD2(x:D2) {}

var a = if true then 1 else 0 // should infer Int
var b : Double = if true then 1 else 0 // should infer Double
var c = if true then 1 else 0.0 // should infer Double
var d = if true then 1.0 else 0 // should infer Double

useInt(a)
useDouble(b)
useDouble(c)
useDouble(d)

var z = if true then a else b // expected-error{{}}

var e = if true then new B else new B // should infer B
var f = if true then new B else new D1 // should infer B
var g = if true then new D1 else new B // should infer B
var h = if true then new D1 else new D1 // should infer D1
var i = if true then new D1 else new D2 // should infer B

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

var x = if MyLogicValue() then 1 else 0
var y = if 22 then 1 else 0 // expected-error{{}}
