// RUN: %target-swift-frontend -typecheck -verify %s

let _: @autodiff (Float) -> Float
let _: @autodiff (Float) throws -> Float

//
// Type differentiability
//

struct NonDiffType { var x: Int }
// FIXME: Properly type-check parameters and the result's differentiability
// expected-error @+1 {{argument is not differentiable, but the enclosing function type is marked '@autodiff'}} {{19-19=@nondiff }}
let _: @autodiff (NonDiffType) -> Float
// expected-error @+1 {{result is not differentiable, but the function type is marked '@autodiff'}}
let _: @autodiff (Float) -> NonDiffType

//
// Argument selection (@nondiff)
//

// expected-error @+1 {{'nondiff' cannot be applied to arguments of a non-differentiable function}}
let _: (@nondiff Float, Float) -> Float

let _: @autodiff (Float, @nondiff Float) -> Float // okay

func foo<T: Differentiable, U: Differentiable>(x: T) -> U {
  let fn: (@autodiff (T) -> U)? = nil
  return fn!(x)
}

func test1<T: Differentiable, U: Differentiable>(_: @autodiff (T) -> @autodiff (U) -> Float) {}
func test2<T: Differentiable, U: Differentiable>(_: @autodiff (T) -> (U) -> Float) {}
// expected-error @+2 {{result is not differentiable, but the function type is marked '@autodiff'}}
// expected-error @+1 {{result is not differentiable, but the function type is marked '@autodiff'}}
func test3<T: Differentiable, U: Differentiable>(_: @autodiff (T) -> @autodiff (U) -> Int) {}
// expected-error @+1 {{result is not differentiable, but the function type is marked '@autodiff'}}
func test4<T: Differentiable, U: Differentiable>(_: @autodiff (T) -> (U) -> Int) {}
