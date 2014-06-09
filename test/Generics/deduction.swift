// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Deduction of generic arguments
//===----------------------------------------------------------------------===//

func identity<T>(value: T) -> T { return value }

func identity2<T>(value: T) -> T { return value }
func identity2<T>(value: T) -> Int { return 0 }

struct X { }
struct Y { }

func useIdentity(x: Int, y: Float, i32: Int32) {
  var x2 = identity(x)
  var y2 = identity(y)

  // Deduction that involves the result type
  x2 = identity(17)
  var i32_2 : Int32 = identity(17)

  // Deduction where the result type and input type can get different results
  // FIXME: Is this actually the behavior we want? It's strange that these
  // two have different behavior.
  var xx : X, yy : Y
  xx = identity(yy) // expected-error{{cannot convert the expression's type '()' to type 'T'}}
  xx = identity2(yy) // expected-error{{could not find an overload for 'identity2' that accepts the supplied arguments}}
}

// FIXME: Crummy diagnostic!
func twoIdentical<T>(x: T, y: T) -> T {}

func useTwoIdentical(xi: Int, yi: Float) {
  var x = xi, y = yi
  x = twoIdentical(x, x)
  y = twoIdentical(y, y)
  x = twoIdentical(x, 1)
  x = twoIdentical(1, x)
  y = twoIdentical(1.0, y)
  y = twoIdentical(y, 1.0)
  
  twoIdentical(x, y) // expected-error{{cannot convert the expression's type '@lvalue Float' to type '@lvalue Int'}}
}

func mySwap<T>(inout x: T,   // expected-note{{in initialization of parameter 'x'}}
               inout y: T) {
  var tmp = x
  x = y
  y = tmp
}

func useSwap(xi: Int, yi: Float) {
  var x = xi, y = yi
  mySwap(&x, &x)
  mySwap(&y, &y)
  
  mySwap(x, x) // expected-error 2{{passing value of type 'Int' to an inout parameter requires explicit '&'}}
  
  mySwap(&x, &y) // expected-error{{'Int' is not identical to 'Float'}}
}

func takeTuples<T, U>(_: (T, U), _: (U, T)) {
}

func useTuples(x: Int, y: Float, z: (Float, Int)) {
  takeTuples((x, y), (y, x))

  takeTuples((x, y), (x, y)) // expected-error{{cannot convert the expression's type '()' to type 'Float'}}

  // FIXME: Use 'z', which requires us to fix our tuple-conversion
  // representation.
}

func acceptFunction<T, U>(f: (T) -> U, t: T, u: U) {}

func passFunction(f: (Int) -> Float, x: Int, y: Float) {
   acceptFunction(f, x, y)
   acceptFunction(f, y, y) // expected-error{{cannot convert the expression's type '()' to type 'Float'}}
}

func returnTuple<T, U>(_: T) -> (T, U) { }

func testReturnTuple(x: Int, y: Float) {
  returnTuple(x) // expected-error{{cannot convert the expression's type '(T, U)' to type 'T'}}
  var rt1 : (Int, Float) = returnTuple(x)
  var rt2 : (Float, Float) = returnTuple(y)
  var rt3 : (Int, Float) = returnTuple(y) // expected-error{{cannot convert the expression's type '(T, U)' to type 'Float'}}
}


func confusingArgAndParam<T, U>(f: (T) -> U, g: (U) -> T) {
  confusingArgAndParam(g, f)
  confusingArgAndParam(f, g)
}

func acceptUnaryFn<T, U>(f: (T) -> U) { }
func acceptUnaryFnSame<T>(f: (T) -> T) { }

func acceptUnaryFnRef<T, U>(inout f: (T) -> U) { }
func acceptUnaryFnSameRef<T>(inout f: (T) -> T) { }

func unaryFnIntInt(_: Int) -> Int {}

func unaryFnOvl(_: Int) -> Int {} // expected-note{{found this candidate}}
func unaryFnOvl(_: Float) -> Int {} // expected-note{{found this candidate}}

// Variable forms of the above functions
var unaryFnIntIntVar : (Int) -> Int = unaryFnIntInt

func passOverloadSet() {
  // Passing a non-generic function to a generic function
  acceptUnaryFn(unaryFnIntInt)
  acceptUnaryFnSame(unaryFnIntInt)

  // Passing an overloaded function set to a generic function
  // FIXME: Yet more terrible diagnostics.
  acceptUnaryFn(unaryFnOvl)  // expected-error{{ambiguous use of 'unaryFnOvl'}}
  acceptUnaryFnSame(unaryFnOvl)

  // Passing a variable of function type to a generic function
  acceptUnaryFn(unaryFnIntIntVar)
  acceptUnaryFnSame(unaryFnIntIntVar)

  // Passing a variable of function type to a generic function to an inout parameter
  acceptUnaryFnRef(&unaryFnIntIntVar)
  acceptUnaryFnSameRef(&unaryFnIntIntVar)

  acceptUnaryFnRef(unaryFnIntIntVar) // expected-error{{passing value of type '(Int) -> Int' to an inout parameter requires explicit '&'}}
}

func acceptFnFloatFloat(f: (Float) -> Float) {}
func acceptFnDoubleDouble(f: (Double) -> Double) {}

func passGeneric() {
  acceptFnFloatFloat(identity)
  acceptFnFloatFloat(identity2)
}

//===----------------------------------------------------------------------===//
// Simple deduction for generic member functions
//===----------------------------------------------------------------------===//
struct SomeType {
  func identity<T>(x: T) -> T { return x }

  func identity2<T>(x: T) -> T { return x } // expected-note 3{{found this candidate}}
  func identity2<T>(x: T) -> Float { } // expected-note 2{{found this candidate}}

  func returnAs<T>() -> T {}
}

func testMemberDeduction(sti: SomeType, ii: Int, fi: Float) {
  var st = sti, i = ii, f = fi
  i = st.identity(i)
  f = st.identity(f)
  i = st.identity2(i)
  f = st.identity2(f) // expected-error{{ambiguous use of 'identity2'}}
  i = st.returnAs()
  f = st.returnAs()
  acceptFnFloatFloat(st.identity) // expected-error{{partial application of struct method is not allowed}}
  acceptFnFloatFloat(st.identity2) // expected-error{{ambiguous use of 'identity2'}}
  acceptFnDoubleDouble(st.identity2) // expected-error{{partial application of struct method is not allowed}}
}

struct StaticFuncs {
  static func chameleon<T>() -> T {}
  func chameleon2<T>() -> T {}
}

struct StaticFuncsGeneric<U> {
  // FIXME: Nested generics are very broken
  // static func chameleon<T>() -> T {}
}

func chameleon<T>() -> T {}

func testStatic(sf: StaticFuncs, sfi: StaticFuncsGeneric<Int>) {
  var x: Int16
  x = StaticFuncs.chameleon()
  x = sf.chameleon2()
  // FIXME: Nested generics are very broken
  // x = sfi.chameleon()
  // typealias SFI = StaticFuncsGeneric<Int>
  // x = SFI.chameleon()
}

//===----------------------------------------------------------------------===//
// Deduction checking for constraints
//===----------------------------------------------------------------------===//
protocol IsBefore {
  func isBefore(other: Self) -> Bool
}

func min2<T : IsBefore>(x: T, y: T) -> T {
  if y.isBefore(x) { return y }
  return x
}

extension Int : IsBefore {
  func isBefore(other: Int) -> Bool { return self < other }
}

func callMin(x: Int, y: Int, a: Float, b: Float) {
  min2(x, y)
  min2(a, b) // expected-error{{type 'Float' does not conform to protocol 'IsBefore'}}
}

func rangeOfIsBefore<R : Generator where R.Element : IsBefore>(range : R) { }


func callRangeOfIsBefore(ia: Int[], da: Double[]) {
  rangeOfIsBefore(ia.generate())
  rangeOfIsBefore(da.generate()) // expected-error{{type 'Double' does not conform to protocol 'IsBefore'}}
}

//===----------------------------------------------------------------------===//
// Deduction for member operators
//===----------------------------------------------------------------------===//
protocol Addable {
  func +(x: Self, y: Self) -> Self
}
func addAddables<T : Addable, U>(x: T, y: T, u: U) -> T {
  u + u // expected-error{{could not find an overload for '+' that accepts the supplied arguments}}
  return x+y
}

//===----------------------------------------------------------------------===//
// Deduction for bound generic types
//===----------------------------------------------------------------------===//
struct MyVector<T> { func size() -> Int {} }

func getVectorSize<T>(v: MyVector<T>) -> Int { // expected-note{{in initialization of parameter 'v'}}
  return v.size()
}

func ovlVector<T>(v: MyVector<T>) -> X {}
func ovlVector<T>(v: MyVector<MyVector<T>>) -> Y {}

func testGetVectorSize(vi: MyVector<Int>, vf: MyVector<Float>) {
  var i : Int
  i = getVectorSize(vi)
  i = getVectorSize(vf)
  // FIXME: $T1 should not show up here!
  getVectorSize(i) // expected-error{{'Int' is not convertible to 'MyVector<T>'}}

  var x : X, y : Y
  x = ovlVector(vi)
  x = ovlVector(vf)
  
  var vvi : MyVector<MyVector<Int>>
  y = ovlVector(vvi)

  var yy = ovlVector(vvi)
  yy = y
  y = yy
}

// <rdar://problem/15104554>
operator postfix <*> {}

protocol MetaFunction {
  typealias Result
  @postfix func <*> (_: Self) -> Result?
}

protocol Bool_ {}
struct False : Bool_ {}
struct True : Bool_ {}

@postfix func <*> <B:Bool_>(_: Test<B>) -> Int? { return .None }
@postfix func <*> (_: Test<True>) -> String? { return .None }

class Test<C: Bool_> : MetaFunction {} // picks first <*>
typealias Inty = Test<True>.Result 
var iy : Inty = 5 // okay, because we picked the first <*>
var iy2 : Inty = "hello" // expected-error{{cannot convert the expression's type 'String' to type 'Inty'}}
