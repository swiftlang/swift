// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{invalid differentiability 'blah' in '@autodiff' attribute; expected 'forward', 'reverse', 'linear', 'constant', or 'bidirectional'}}
let _: @autodiff(blah) (Float) -> Float

// FIXME: The type checker thinks "Float" is the differentiability name. This
// means something's wrong in the parser.
//
// let _: @autodiff (Float) -> Float

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
