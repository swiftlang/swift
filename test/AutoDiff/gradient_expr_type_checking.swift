// RUN: %target-swift-frontend -typecheck -verify %s

func foo(x: Float, y: Float) -> Float {}

let someVar: Int = 100

#gradient(someVar) // expected-error {{only functions can be differentiated}}
#valueAndGradient(someVar) // expected-error {{only functions can be differentiated}}

let _ = #gradient(foo) // ok
let _ = #valueAndGradient(foo) // ok
let _: (Float, Float) -> (Float, Float) = #gradient(foo) // ok
let _: (Float, Float) -> (value: Float, gradient: (Float, Float)) = #valueAndGradient(foo) // ok

let _: (Float, Float) -> Float = #gradient(foo, wrt: .0) // ok
let _: (Float, Float) -> (value: Float, gradient: Float)
  = #valueAndGradient(foo, wrt: .0) // ok
let _: (Float, Float) -> (Float, Float) = #gradient(foo, wrt: .0, .1) // ok
let _: (Float, Float) -> (Float, Float, Float) = #gradient(foo, wrt: .0, .1, .2) // expected-error {{the parameter index is out of bounds for type}}

// S is not a valid differentiation parameter because it does not conform to FloatingPoint.
struct S {
  func a(_ x: Float) -> Float {}

  lazy var da = #gradient(self.a) // ok
  lazy var daWithValue = #valueAndGradient(self.a) // ok
  lazy var da2: (Float) -> Float = #gradient(self.a) // ok
  lazy var daWithValue2: (Float) -> (value: Float, gradient: Float) = #valueAndGradient(self.a) // ok

  func b() {
    let _: (Float) -> Float = #gradient(a) // ok
  }

  static func c(_ x: Float) -> Float {}
}

let s = S()
let _: (Float) -> Float = #gradient(s.a)
let _: (Float) -> (Float, Float) = #valueAndGradient(s.a)
let _: (Double) -> Double = #gradient(s.a)
// expected-error @-1 {{cannot convert value of type '(Float) -> Float' to specified type}}
let _: (Double) -> (Double, Double) = #valueAndGradient(s.a)
// expected-error @-1 {{cannot convert value of type '(Float) -> (value: Float, gradient: Float)' to specified type}}

// Gradient expressions with generic primal.
func e<T>(_ x: T) -> T {} // expected-note 2 {{in call to function 'e'}}

let _ = #gradient(e) // expected-error {{generic parameter 'T' could not be inferred}}
let _ = #valueAndGradient(e) // expected-error {{generic parameter 'T' could not be inferred}}
let _: (Float) -> Float = #gradient(e)
let _: (Double) -> Double = #gradient(e)
let _: (Float) -> (Float, Float) = #valueAndGradient(e)
let _: (Double) -> (Double, Double) = #valueAndGradient(e)

let _: (Float) -> (Float, Float, Float) = #gradient(e) // expected-error {{cannot convert gradient expression to incompatible contextual type}}
let _: ((Float, Float)) -> (Float, Float) = #gradient(e) // expected-error {{gradient parameter has non-differentiable type '(Float, Float)'}}
let _: (Int) -> (Int) = #gradient(e) // expected-error {{gradient parameter has non-differentiable type 'Int'}}
let _: (Float) -> Double = #gradient(e) // expected-error {{cannot convert gradient expression to incompatible contextual type}}

// Complex type inference.
func add<T : FloatingPoint>(_ x: T, _ y: T) -> T {
  return x + y
}
func dadd<T : FloatingPoint>(_ x: T, _ y: T) -> (T, T) {
  return #gradient(add)(x, y) // ok
}
func daddWithValue<T : FloatingPoint>(
  _ x: T, _ y: T
) -> (value: T, gradient: (T, T)) {
  return #valueAndGradient(add)(x, y) // ok
}

func addNumeric<T : Numeric>(_ x: T, _ y: T) -> T {
  return x + y
}
func daddNumeric<T : Numeric>(_ x: T, _ y: T) -> (T, T) {
  return #gradient(addNumeric)(x, y) // expected-error {{gradient parameter has non-differentiable type 'T'}}
}
// Ok because the constraint on daddNumeric is FloatingPoint, not Numeric.
func daddFloatingPoint<T : FloatingPoint>(_ x: T, _ y: T) -> (T, T) {
  return #gradient(addNumeric)(x, y) // ok
}
// Ok because the constraint on daddNumeric is FloatingPoint, not Numeric.
func daddFloatingPointWithValue<T : FloatingPoint>(_ x: T, _ y: T) -> (value: T, gradient: (T, T)) {
  return #valueAndGradient(addNumeric)(x, y) // ok
}
