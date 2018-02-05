// RUN: %target-swift-frontend -typecheck -verify %s

@differentiable(gradient: dfoo) // expected-error {{@differentiable may only be used on 'func' declarations}}
let x: Float = 1

@differentiable(gradient: dfoo) // expected-error {{@differentiable may only be used on 'func' declarations}}
protocol P {}

func dfoo(_ x: Float, primal: Float, seed: Float) -> Float {
  return 2 * x
}

@differentiable(gradient: dfoo(_:primal:seed:)) // ok!
func foo(_ x: Float) -> Float {
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

@differentiable(gradient: dfoo2_wrong_type(_:primal:seed:)) // expected-error {{'dfoo2_wrong_type(_:primal:seed:)' does not have an overload with expected type '(Float, Float, Float) -> Float'}}
func foo2(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: dfoo(_:primal:_:)) // expected-error {{undefined identifier 'dfoo(_:primal:_:)' as gradients function}}
func foo3(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: meow) // expected-error {{undefined identifier 'meow' as gradients function}}
func foo4(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: woof) // expected-error {{'foo5' has no arguments to differentiate with respect to}}
func foo5() -> Float {
  return 1
}

@differentiable(withRespectTo: (self, .0, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' argument is only applicable to instance methods}}
func meow1(_ x: Float, _: Float) -> Float {
  return 1 + x
}

func dmeow1_in_S(_ s: S, _ x: Float, _ primal: Float, _ seed: Float) -> (S, Float) {
  return (s, x)
}

struct S {
  @differentiable(withRespectTo: (self, .0), gradient: dmeow1_in_S(_:_:_:_:)) // ok
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
}

@differentiable(withRespectTo: (.1, .2), gradient: dmeow2(_:_:_:_:_:)) // ok
func meow2(_ x: Float, _: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(withRespectTo: (.2, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{argument indices must be ascending}}
func meow3(_ x: Float, _: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(withRespectTo: (.2, self, .1), gradient: dmeow1(_:_:_:_:)) // expected-error {{'self' argument is only applicable to instance methods}}
func meow4(_ x: Float, _: Float, _: Float) -> Float {
  return 1 + x
}

func dmeow1(_ x: Float, _: Float, _: Float, _: Float) -> (Float, Float) {
  return (x, x)
}

func dmeow2(_ x: Float, _: Float, _: Float, _: Float, _: Float) -> (Float, Float) {
  return (x, x)
}
