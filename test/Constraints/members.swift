// RUN: %target-parse-verify-swift

import Swift

struct X {
  func f0(i: Int) -> X { }

  func f1(i: Int) { }
  mutating
  func f1(f: Float) { }

  func f2<T>(x: T) -> T { }
}

struct Y<T> {
  func f0(_: T) -> T {}
  func f1<U>(x: U, y: T) -> (T, U) {}
}

var i : Int
var x : X
var yf : Y<Float>

func g0(_: (inout X) -> (Float) -> ()) {}

x.f0(i)
x.f0(i).f1(i)
g0(X.f1)
x.f0(x.f2(1))
x.f0(1).f2(i)
yf.f0(1)
yf.f1(i, y: 1)

// Module
Swift.print(3)

var format : String
format._splitFirstIf({ $0.isASCII() })

// Archetypes
func doGetLogicValue<T : BooleanType>(t: T) {
  t.boolValue
}

// Members referenced from inside the class
struct Z {
  var i : Int
  func getI() -> Int { return i }

  func curried(x: Int)(y: Int) -> Int { return x + y }

  subscript (k : Int) -> Int {
    get {
      return i + k
    }
    mutating
    set {
      i -= k
    }
  }
}

struct GZ<T> {
  var i : T
  func getI() -> T { return i }

  func f1<U>(a: T, b: U) -> (T, U) { 
    return (a, b)
  }
  
  func f2() {
    var f : Float
    var t = f1(i, b: f)
    f = t.1

    var zi = Z.i; // expected-error{{'Z.Type' does not have a member named 'i'}}
  }
}

// Members of literals
// FIXME: Crappy diagnostic
"foo".lower() // expected-error{{'String' does not have a member named 'lower'}}
var tmp = "foo".debugDescription

enum W {
  case Omega

  func foo(x: Int) {}
  func curried(x: Int)(y: Int) {}
}

// Partial applications of struct or enum methods are not allowed
var z = Z(i: 0)
var getI = z.getI // expected-error{{partial application of struct method is not allowed}}
var zi = z.getI()
var zcurried1 = z.curried // expected-error{{partial application of struct method is not allowed}}
var zcurried2 = z.curried(0) // expected-error{{partial application of struct method is not allowed}}
var zcurriedFull = z.curried(0)(y: 1)

var w = W.Omega
var foo = w.foo // expected-error{{partial application of enum method is not allowed}}
var fooFull : () = w.foo(0)
var wcurried1 = w.curried // expected-error{{partial application of enum method is not allowed}}
var wcurried2 = w.curried(0) // expected-error{{partial application of enum method is not allowed}}
var wcurriedFull : () = w.curried(0)(y: 1)

// Member of enum Type
func enumMetatypeMember(opt: Int?) {
  opt.None // expected-error{{'Int?' does not have a member named 'None'}}
}

// Reference a Type member. <rdar://problem/15034920>
class G<T> {
  class In { // expected-error{{nested in generic type}}
    class func foo() {}
  }
}

func goo() {
  G<Int>.In.foo()
}

protocol P {
  func bar(x: Int)
}

@objc
protocol P2 {
  func bar(x: Int)
}


func generic<T: P>(t: T) {
  var bar = t.bar // expected-error{{partial application of generic method is not allowed}}
  var barFull : () = t.bar(0)
}

func existential(p: P, p2 : P2) {
  var bar = p.bar // expected-error{{partial application of protocol method is not allowed}}
  var bar2 = p2.bar // expected-error{{partial application of protocol method is not allowed}}
  var barFull : () = p.bar(0)
}

protocol ClassP : class {
  func bas(x: Int)
}

func genericClassP<T: ClassP>(t: T) {
  var bas = t.bas
  var barFull : () = t.bas(0)
}

func existentialClassP(p: ClassP) {
  var bas = p.bas  // expected-error{{partial application of protocol method is not allowed}}
  var basFull : () = p.bas(0)
}

// <rdar://problem/15537772>
struct DefaultArgs {
  static func f(a: Int = 0) -> DefaultArgs {
    return DefaultArgs()
  }
  init() {
    self = .f()
  }
}


class InstanceOrClassMethod {
  func method() -> Bool { return true }
  class func method(other: InstanceOrClassMethod) -> Bool { return false }
}

func testPreferClassMethodToCurriedInstanceMethod(obj: InstanceOrClassMethod) {
  let result = InstanceOrClassMethod.method(obj)
  let resultChecked: Bool = result // no-warning
  let curried: () -> Bool = InstanceOrClassMethod.method(obj)
}

protocol Numeric {
  func +(x: Self, y: Self) -> Self
}

func acceptBinaryFunc<T>(x: T, fn: (T, T) -> T) { }

func testNumeric<T : Numeric>(x: T) {
  acceptBinaryFunc(x, +) // expected-error{{partial application of generic method is not allowed}}
}

/* FIXME: We can't check this directly, but it can happen with
multiple modules.

class PropertyOrMethod {
  func member() -> Int { return 0 }
  let member = false

  class func methodOnClass(obj: PropertyOrMethod) -> Int { return 0 }
  let methodOnClass = false
}

func testPreferPropertyToMethod(obj: PropertyOrMethod) {
  let result = obj.member
  let resultChecked: Bool = result
  let called = obj.member()
  let calledChecked: Int = called
  let curried = obj.member as () -> Int

  let methodOnClass = PropertyOrMethod.methodOnClass
  let methodOnClassChecked: (PropertyOrMethod) -> Int = methodOnClass
}
*/
