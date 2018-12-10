// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{invalid differentiability 'blah' in '@autodiff' attribute; expected 'forward', 'reverse', 'linear', 'constant', or 'bidirectional'}}
let _: @autodiff(blah) (Float) -> Float

let _: @autodiff (Float) -> Float
let _: @autodiff(bidirectional) (Float) -> Float
let _: @autodiff(forward) (Float) -> Float
let _: @autodiff(reverse) (Float) -> Float
let _: @autodiff(linear) (Float) -> Float
let _: @autodiff(const) (Float) -> Float
let _: @autodiff(bidirectional) (Float) -> Float

let _: @autodiff(bidirectional, order: 3) (Float) -> Float
let _: @autodiff(forward, order: 4) (Float) -> Float
let _: @autodiff(reverse, order: 5) (Float) -> Float

// expected-error @+1 {{differentiation order cannot be specified in 'linear' mode}}
let _: @autodiff(linear, order: 6) (Float) -> Float
// expected-error @+1 {{differentiation order cannot be specified in 'const' mode}}
let _: @autodiff(const, order: 7) (Float) -> Float

// expected-error @+1 {{differentiation order cannot be zero; it should be at least first-order}}
let _: @autodiff(order: 0) (Float) -> Float
// expected-error @+1 {{differentiation order cannot be zero; it should be at least first-order}}
let _: @autodiff(forward, order: 0) (Float) -> Float

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

