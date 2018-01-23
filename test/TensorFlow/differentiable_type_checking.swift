// RUN: %target-swift-frontend -typecheck -verify %s

@differentiable(gradient: dfoo) // expected-error {{@differentiable may only be used on 'func' declarations}}
let x: Float = 1

@differentiable(gradient: dfoo) // expected-error {{@differentiable may only be used on 'func' declarations}}
protocol P {}

func dfoo(_ x: Float, primal: Float, seed: Float) -> Float {
  return 2 * x
}

@differentiable(gradient: dfoo(_:primal:_:)) // expected-error {{cannot resolve gradient function 'dfoo(_:primal:_:)'}}
func foo2(_ x: Float) -> Float {
  return x * x
}

@differentiable(gradient: meow) // expected-error {{cannot resolve gradient function 'meow'}}
func foo3(_ x: Float) -> Float {
  return x * x
}
