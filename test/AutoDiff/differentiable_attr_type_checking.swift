// RUN: %target-swift-frontend -typecheck -verify %s

@differentiable(gradient: dfoo) // expected-error {{@differentiable may only be used on 'func' declarations}}
let x: Float = 1

@differentiable(gradient: dfoo) // expected-error {{@differentiable may only be used on 'func' declarations}}
protocol P {}

func dfoo(_ x: Float, primal: Float, seed: Float) -> Float { // expected-note {{did you mean 'dfoo'?}}
  return 2 * x
}

@differentiable(gradient: dfoo(_:primal:seed:)) // ok!
func foo(_ x: Float) -> Float { // expected-note {{did you mean 'foo'?}}
  return x * x
}

func dbar(_ x: Float, _ y: Float, primal: Float, seed: Float) -> (Float, Float) {
  return (1, 1)
}

@differentiable(gradient: dbar(_:_:primal:seed:)) // ok!
func bar(_ x: Float, _ y: Float) -> Float {
  return x + y
}

func dfoo2_wrong_type(_ x: Float, primal: Float, seed: Double) -> Float {
  return 2 * x
}

@differentiable(gradient: dfoo2_wrong_type(_:primal:seed:)) // expected-error {{'dfoo2_wrong_type(_:primal:seed:)' does not have expected type '(Float, Float, Float) -> Float'}}
func foo2(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: dfoo(_:primal:_:)) // expected-error {{use of unresolved identifier 'dfoo(_:primal:_:)'}}
func foo3(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: meow) // expected-error {{use of unresolved identifier}}
func foo4(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: woof) // expected-error {{'foo5' has no parameters to differentiate with respect to}}
func foo5() -> Float {
  return 1
}

@differentiable(withRespectTo: (self, .0, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' parameter is only applicable to instance methods}}
func meow1(_ x: Float, _: Float) -> Float { // expected-note {{did you mean}}
  return 1 + x
}

func dmeow1_out_of_S(_ s: S, _ x: Float, _ primal: Float, _ seed: Float) -> (S, Float) {
  return (s, x)
}

func dPlus(_ x: S, _ y: S, _ primal: S, _ seed: S) -> (S, S) {
  return (x, y)
}

func dPlus_curried(_ x: S.Type) -> (S, S, S, S) -> (S, S) {
  fatalError("never run")
}

func dPlus(_ x: Int, _ y: S, _ primal: S, _ seed: S) -> (Int, S) {
  return (x, y)
}

struct C {
  @differentiable(gradient: adjoint)
  func primal(x: Float) -> Float {
    return x
  }

  func adjoint(x: Float, _: Float, _: Float) -> Float {
    return x
  }
}

struct S {
  @differentiable(withRespectTo: (self, .0), gradient: dmeow1_out_of_S(_:_:_:_:)) // expected-error {{'dmeow1_out_of_S' is not defined in the current declaration context}}
  func meow1(_ x: Float) -> Float {
    return x + 1
  }

  @differentiable(withRespectTo: (.1, self, .2), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' parameter must come first in the parameter list}}
  func meow1_not_ok(_ x: Float, _: Float, _: Float) -> Float {
    return 1 + x
  }

  @differentiable(withRespectTo: (self, .0), gradient: dmeow1_in_S(_:_:_:_:)) // expected-error {{'self' parameter is only applicable to instance methods}}
  static func meow1(_ x: Float) -> Float {
    return x + 1
  }

  @differentiable(withRespectTo: (self, .0, .1), gradient: dPlus(_:_:_:_:)) // expected-error {{'self' parameter is only applicable to instance methods}}
  static func + (lhs: S, rhs: S) -> S {
    return lhs
  }

  @differentiable(withRespectTo: (.0, .1), gradient: dPlus(_:_:_:_:)) // expected-error {{ambiguous or overloaded identifier 'dPlus' cannot be used in @differentiable attribute}}
  static func - (lhs: S, rhs: S) -> S {
    return lhs
  }

  @differentiable(withRespectTo: (.0, .1), gradient: dPlus_curried(_:)) // expected-error {{'dPlus_curried' is not defined in the current declaration context}}
  static func try_plus_curried_adjoint(lhs: S, rhs: S) -> S {
    return lhs
  }

  static func dMul(_ lhs: Int, _ rhs: S, _: S, _: S) -> (Int, S) {
    return (lhs, rhs)
  }

  @differentiable(withRespectTo: (.0, .1), gradient: dMul) // ok
  static func * (lhs: Int, rhs: S) -> S {
    return rhs
  }

  @differentiable(withRespectTo: (.0, .1), gradient: dMul) // expected-error {{use of unresolved identifier 'dMul'}}
  func instance_mul(lhs: Int, rhs: S) -> S {
    return rhs
  }
}

@differentiable(withRespectTo: (.1, .2), gradient: dmeow2(_:_:_:_:_:)) // ok
func meow2(_ x: Float, _: Float, _: Float) -> Float { // expected-note {{did you mean}}
  return 1 + x
}

@differentiable(withRespectTo: (.2, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{parameter indices must be ascending}}
func meow3(_ x: Float, _: Float, _: Float) -> Float { // expected-note {{did you mean}}
  return 1 + x
}

@differentiable(withRespectTo: (.2, self, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' parameter is only applicable to instance methods}}
func meow4(_ x: Float, _: Float, _: Float) -> Float { // expected-note {{did you mean}}
  return 1 + x
}

func dmeow1(_ x: Float, _: Float, _: Float, _: Float) -> (Float, Float) {
  return (x, x)
}

func dmeow2(_ x: Float, _: Float, _: Float, _: Float, _: Float) -> (Float, Float) {
  return (x, x)
}

// Test cross-declaration references.
// NOTE: Cross-declaration references across files is not tested because it is
// difficult to set up and requires creating a new test target. Since the
// TensorFlow library will declare primals/adjoints in separate files,
// successful compilation of the library itself is a sufficient test.

// Primal in struct definition, adjoint in extension.
struct E1 {
  @differentiable(gradient: adjoint)
  func primal(x: Float) -> Float {
    return x
  }
}
extension E1 {
  func adjoint(x: Float, _: Float, _: Float) -> Float {
    return x
  }
}

// Primal and adjoint in separate struct extensions.
struct E2 {}
extension E2 {
  @differentiable(gradient: adjoint)
  func primal(x: Float) -> Float {
    return x
  }
}
extension E2 {
  func adjoint(x: Float, _: Float, _: Float) -> Float {
    return x
  }
}

// Primal and adjoint in separate struct extensions, with matching generic
// constraints.
struct E3<T> {}
extension E3 where T == Float {
  @differentiable(gradient: adjoint_same_constraint)
  func primal(x: Float) -> Float {
    return x
  }
}
extension E3 where T == Float {
  func adjoint_same_constraint(x: Float, _: Float, _: Float) -> Float {
    return x
  }
}

struct E4<T> {}
extension E4 {
  @differentiable(gradient: adjoint_no_constraint)
  func primal(x: Float) -> Float {
    return x
  }
}
extension E4 {
  func adjoint_no_constraint(x: Float, _: Float, _: Float) -> Float {
    return x
  }
}

// Primal and adjoint in separate struct extensions, with non-matching
// generic constraints.
struct E5<T> {}
extension E5 {
  @differentiable(gradient: adjoint_diff_constraint)
  // expected-error @-1 {{'adjoint_diff_constraint' does not have expected type '<T> (E5<T>) -> (Float, Float, Float) -> Float'}}
  func primal(x: Float) -> Float {
    return x
  }
}
extension E5 where T == Float {
  func adjoint_diff_constraint(x: Float, _: Float, _: Float) -> Float {
    return x
  }
}

// Primal and adjoint in separate struct extensions, differentiating with
// respect to self.
struct E6<T> {}
extension E6 {
  @differentiable(withRespectTo: (self), gradient: adjoint_wrt_self)
  func primal123(x: Float) -> Float {
    return x
  }
}
extension E6 {
  func adjoint_wrt_self(x: Float, _: Float, _: Float) -> E6 {
    return self
  }
}

// Generic functions with no constraints.
func dbaz1<T>(_ x: T, _ y: T, primal: T, seed: T) -> (T, T) {
  return (y, x)
}
@differentiable(gradient: dbaz1(_:_:primal:seed:)) // ok!
func baz1<T>(_ x: T, _ y: T) -> T {
  return x
}

// Generic functions with matching constraints.
func dbaz2<T : FloatingPoint>(_ x: T, _ y: T, primal: T, seed: T) -> (T, T) {
  return (1, 1)
}
@differentiable(gradient: dbaz2(_:_:primal:seed:)) // ok!
func baz2<T : FloatingPoint>(_ x: T, _ y: T) -> T {
  return x + y
}

// Generic functions with different constraints.
func dbaz3<T : Numeric>(_ x: T, _ y: T, primal: T, seed: T) -> (T, T) {
  return (1, 1)
}
@differentiable(gradient: dbaz3(_:_:primal:seed:))
// expected-error @-1 {{'dbaz3(_:_:primal:seed:)' does not have expected type '<T where T : FloatingPoint> (T, T, T, T) -> (T, T)'}}
func baz3<T : FloatingPoint>(_ x: T, _ y: T) -> T {
  return x + y
}
