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

@differentiable(gradient: woof) // expected-error {{'foo5' has no arguments to differentiate with respect to}}
func foo5() -> Float {
  return 1
}

@differentiable(withRespectTo: (self, .0, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' argument is only applicable to instance methods}}
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

  @differentiable(withRespectTo: (.1, self, .2), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' argument must come first in the argument list}}
  func meow1_not_ok(_ x: Float, _: Float, _: Float) -> Float {
    return 1 + x
  }

  @differentiable(withRespectTo: (self, .0), gradient: dmeow1_in_S(_:_:_:_:)) // expected-error {{'self' argument is only applicable to instance methods}}
  static func meow1(_ x: Float) -> Float {
    return x + 1
  }

  @differentiable(withRespectTo: (self, .0, .1), gradient: dPlus(_:_:_:_:)) // expected-error {{'self' argument is only applicable to instance methods}}
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

@differentiable(withRespectTo: (.2, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{argument indices must be ascending}}
func meow3(_ x: Float, _: Float, _: Float) -> Float { // expected-note {{did you mean}}
  return 1 + x
}

@differentiable(withRespectTo: (.2, self, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' argument is only applicable to instance methods}}
func meow4(_ x: Float, _: Float, _: Float) -> Float { // expected-note {{did you mean}}
  return 1 + x
}

func dmeow1(_ x: Float, _: Float, _: Float, _: Float) -> (Float, Float) {
  return (x, x)
}

func dmeow2(_ x: Float, _: Float, _: Float, _: Float, _: Float) -> (Float, Float) {
  return (x, x)
}
