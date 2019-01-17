// RUN: %target-swift-frontend -typecheck -verify %s

let _: @autodiff (Float) -> Float
let _: @autodiff (Float) throws -> Float

//
// Type differentiability
//

struct NonDiffType { var x: Int }
// FIXME: Properly type-check parameters and the result's differentiability
// xpected-error @+1 {{argument is not differentiable, but the enclosing function type is marked '@autodiff'; did you want to add '@nondiff' to this argument?}}
let _: @autodiff (NonDiffType) -> Float
// xpected-error @+1 {{result is not differentiable, but the enclosing function type is marked '@autodiff'; did you want to add '@nondiff' to this argument?}}
let _: @autodiff (Float) -> NonDiffType

//
// Argument selection (@nondiff)
//

// expected-error @+1 {{'nondiff' cannot be applied to arguments of a non-differentiable function}}
let _: (@nondiff Float, Float) -> Float

let _: @autodiff (Float, @nondiff Float) -> Float // okay

