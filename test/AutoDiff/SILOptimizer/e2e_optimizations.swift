// RUN: 

import _Differentiation

func foo(_ x: Float) -> Float { x }

@inline(never)
func consume<T>(_ x: T) { x }

func testDifferentiableFunctionConversion() {
  let diffFoo: @differentiable (Float) -> Float = foo
  consume(diffFoo)
}
