// RUN: %target-swift-frontend -typecheck -verify %s

func foo(x: Float, y: Float) -> Float {}

let someVar: Int = 100

#gradient(of: someVar) // expected-error {{only functions can be differentiated}}
#valueAndGradient(of: someVar) // expected-error {{only functions can be differentiated}}

let _ = gradient(of: foo) // ok
let _ = valueAndGradient(of: foo) // ok
let _: (Float, Float) -> (Float, Float) = gradient(of: foo) // ok
let _: (Float, Float) -> (value: Float, gradient: (Float, Float)) = valueAndGradient(of: foo) // ok

let _: (Float, Float) -> Float = #gradient(of: foo, withRespectTo: .0) // ok
let _: (Float, Float) -> (value: Float, gradient: Float)
  = #valueAndGradient(of: foo, withRespectTo: .0) // ok
let _: (Float, Float) -> (Float, Float) = #gradient(of: foo, withRespectTo: self, .0) // expected-error {{a 'self' parameter can only be used in an instance declaration}}
let _: (Float, Float) -> (Float, Float) = #gradient(of: foo, withRespectTo: self, .0) // expected-error {{a 'self' parameter can only be used in an instance declaration}}
let _: (Float, Float) -> (Float, Float) = #gradient(of: foo, withRespectTo: .0, self) // expected-error {{the 'self' parameter must be the first}}
let _: (Float, Float) -> (Float, Float) = #gradient(of: foo, withRespectTo: .0, .1) // ok
let _: (Float, Float) -> (Float, Float, Float) = #gradient(of: foo, withRespectTo: .0, .1, .2) // expected-error {{the parameter index is out of bounds for type}}

// S is not a valid differentiation parameter because it does not conform to FloatingPoint.
struct S {
  func a(_ x: Float) -> Float {}

  lazy var da = #gradient(of: self.a) // ok
  lazy var daWithValue = #valueAndGradient(of: self.a) // ok
  lazy var da2: (Float) -> Float = #gradient(of: self.a) // ok
  lazy var daWithValue2: (Float) -> (value: Float, gradient: Float) = #valueAndGradient(of: self.a) // ok

  func b() {
    let _: (Float) -> Float = #gradient(of: a) // ok
    let _: (Float) -> S = #gradient(of: a, withRespectTo: self) // expected-error {{gradient parameter has non-differentiable type 'S'}}
  }

  static func c(_ x: Float) -> Float {}

  static func d() -> Float {
    let _: (Float) -> S = #gradient(of: c, withRespectTo: self) // expected-error {{a 'self' parameter can only be used in an instance declaration context}}
  }
}

let s = S()
let _: (Float) -> Float = gradient(of: s.a)
let _: (Float) -> (Float, Float) = valueAndGradient(of: s.a)
let _: (Double) -> Double = gradient(of: s.a)
// expected-error @-1 {{cannot convert value of type '(Float) -> Float' to specified type}}
let _: (Double) -> (Double, Double) = valueAndGradient(of: s.a)
// expected-error @-1 {{cannot convert value of type '(Float) -> (value: Float, gradient: Float)' to specified type}}

// Gradient expressions with generic primal.
func e<T>(_ x: T) -> T {} // expected-note 2 {{in call to function 'e'}}

let _ = #gradient(of: e) // expected-error {{generic parameter 'T' could not be inferred}}
let _ = #valueAndGradient(of: e) // expected-error {{generic parameter 'T' could not be inferred}}
let _: (Float) -> Float = #gradient(of: e)
let _: (Double) -> Double = #gradient(of: e)
let _: (Float) -> (Float, Float) = #valueAndGradient(of: e)
let _: (Double) -> (Double, Double) = #valueAndGradient(of: e)

let _: (Float) -> (Float, Float, Float) = #gradient(of: e) // expected-error {{cannot convert gradient expression to incompatible contextual type}}
let _: ((Float, Float)) -> (Float, Float) = #gradient(of: e) // expected-error {{gradient parameter has non-differentiable type '(Float, Float)'}}
let _: (Int) -> (Int) = #gradient(of: e) // expected-error {{gradient parameter has non-differentiable type 'Int'}}
let _: (Float) -> Double = #gradient(of: e) // expected-error {{cannot convert gradient expression to incompatible contextual type}}

// Complex type inference.
func add<T : FloatingPoint>(_ x: T, _ y: T) -> T {
  return x + y
}
func dadd<T : FloatingPoint>(_ x: T, _ y: T) -> (T, T) {
  return #gradient(of: add)(x, y) // ok
}
func daddWithValue<T : FloatingPoint>(
  _ x: T, _ y: T
) -> (value: T, gradient: (T, T)) {
  return #valueAndGradient(of: add)(x, y) // ok
}

func addNumeric<T : Numeric>(_ x: T, _ y: T) -> T {
  return x + y
}
func daddNumeric<T : Numeric>(_ x: T, _ y: T) -> (T, T) {
  return #gradient(of: addNumeric)(x, y) // expected-error {{gradient parameter has non-differentiable type 'T'}}
}
// Ok because the constraint on daddNumeric is FloatingPoint, not Numeric.
func daddFloatingPoint<T : FloatingPoint>(_ x: T, _ y: T) -> (T, T) {
  return #gradient(of: addNumeric)(x, y) // ok
}
// Ok because the constraint on daddNumeric is FloatingPoint, not Numeric.
func daddFloatingPointWithValue<T : FloatingPoint>(_ x: T, _ y: T) -> (value: T, gradient: (T, T)) {
  return #valueAndGradient(of: addNumeric)(x, y) // ok
}
