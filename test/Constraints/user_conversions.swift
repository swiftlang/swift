// RUN: %swift -parse -verify %s

struct Y { }

class A { 
  @conversion func __conversion() -> Y {}
}

class B : A { }

struct X { 
  @conversion func __conversion() -> B {}
}

func fb(_: B) {}
func fa(_: A) {}
func fy(_: Y) {}

var a : A
var b : B
var x : X
var y : Y

fb(x)
fa(x)
fy(b)

// FIXME: Awful diagnostic.
fy(x) // expected-error{{cannot convert the expression's type '()' to type 'Y'}}


func do_auto_close(f: @auto_closure () -> Y) {}
do_auto_close(b)

struct Wrapper<T> {
  @conversion func __conversion() -> T { }
  @conversion func __conversion() -> (T, Bool) { }
}

func testWrapper(wi: Wrapper<Int>) {
  var i : Int = wi
  var ib : (Int, Bool) = wi
}

struct AnyT {
  @conversion func __conversion<T>() -> T { }
}

struct AnyPair {
  @conversion func __conversion<T>() -> (T, T) { }
}

func testAny(at: AnyT, let ap: AnyPair) {
  var i : Int = at
  var f : Float = at
  var t1 : (Int, Int) = at
  t1 = ap
  i = ap // expected-error{{'(T, T)' is not a subtype of 'Int'}}
}

// Prefer solutions with fewer user conversions.
func f1(b: B) -> Int { return 0 }
func f1(x: X) -> Double { return 0.0 }

func f2(b1: B, b2: B) -> Int { return 0 }
func f2(x1: X, b2: B) -> Double { return 0.0 }
func f2(b1: B, x2: X) -> UnicodeScalar { return "a" }
func f2(x1: X, x2: X) -> String { return "" }

func testWeight(b: B, x: X) {
  var d : Double
  var i : Int
  var c : UnicodeScalar
  var s : String

  // Simple case: one conversion
  var f1_x = f1(x)
  d = f1_x
  var f1_b = f1(b)
  i = f1_b

  // Two conversions
  var f2_b_b = f2(b, b)
  i = f2_b_b
  var f2_x_b = f2(x, b)
  d = f2_x_b
  var f2_b_x = f2(b, x)
  c = f2_b_x
  var f2_x_x = f2(x, x)
  s = f2_x_x
}

// Mutually convertible
struct Mut1 {
  @conversion func __conversion() -> Mut2 {}
}

struct Mut2 {
  @conversion func __conversion() -> Mut1 {}
}

func f3(m: Mut1) -> Int { }
func f3(m: Mut2) -> Double { }

func mutual(m1: Mut1, m2: Mut2) {
  var i : Int
  var d : Double

  var f3_m1 = f3(m1)
  i = f3_m1

  var f3_m2 = f3(m2)
  d = f3_m2
}

extension Float {
  @conversion func __conversion() -> Double { return Double(self) }
}

func floatConvert() {
  var f : Float = 3.14159
  var d : Double = 2.71828
  var cd : Double = f
}

// User-defined conversion based on contextual type.
struct Proxy<T> {
  @conversion func __conversion() -> T { return value[0] }
  var value: T[]
}

class OutputStreamWrapper<T> {
  var proxy: Proxy<T>

  init(proxy: Proxy<T>) {
    self.proxy = proxy
  }

  @conversion func __conversion() -> T {
    return proxy
  }
}

// User-defined conversions on archetypes.
protocol Convertible {
  typealias TargetType
  @conversion func __conversion() -> TargetType
}

struct ConvProxy<T:Convertible> {
  @conversion func __conversion() -> T.TargetType { return value }
  var value : T
}

// Test nil conversions
func takes_unsafe_pointer(x: UnsafePointer<Int>) { }
takes_unsafe_pointer(nil)

// User-defined conversions vs. superclass conversions.
protocol P { }

class GenericBase<T> { }

class GenericDerived<U> : GenericBase<U[]> {
  @conversion func __conversion() -> GenericBase<U> { }
}

extension Int : P { }

func acceptAnyGenericBase<T : P>(value: GenericBase<T>) { }

func testSuperclassConversion(yi: GenericDerived<Int>) {
  acceptAnyGenericBase(yi)
}

// User-defined conversions vs. tuple conversions.
struct Pair<T, U> {
  var t : T, u : U
  @conversion func __conversion() -> (T, U) {
    return (t, u)
  }
}

func testPair(p: Pair<Int, B>) {
  var p1 : (Int, B) = p
  var p2 : (Int, A) = p // expected-error{{cannot express tuple conversion '(Int, B)' to '(Int, A)'}}

  var labeled_p : (pair : Pair<Int, B>) = p
  p1 = labeled_p
  p2 = labeled_p // expected-error{{cannot express tuple conversion '(Int, B)' to '(Int, A)'}}
}
