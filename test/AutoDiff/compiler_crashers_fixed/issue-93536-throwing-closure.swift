// RUN: %target-swift-frontend -emit-sil -module-name test -verify %s

// A @differentiable(reverse) function that takes a THROWING @differentiable closure
// parameter and has NO registered @derivative. The compiler must synthesize the derivative
// and crashes doing so. It does not matter whether the parameter's error is generic
// (`throws(E)`), concrete typed (`throws(E0)`), or untyped (`throws`); nor whether anything
// calls `apply` — the differentiability witness alone is enough.
import _Differentiation

struct E0: Error {}

// generic error — crashes
@differentiable(reverse)
func applyGeneric<E: Error>(_ x: Double, _ body: @differentiable(reverse) (Double) throws(E) -> Double) throws(E) -> Double {
    try body(x) * 2
}

// concrete typed error — crashes
@differentiable(reverse)
func applyTyped(_ x: Double, _ body: @differentiable(reverse) (Double) throws(E0) -> Double) throws(E0) -> Double {
    try body(x) * 2
}

// untyped `throws` — crashes
@differentiable(reverse)
func applyUntyped(_ x: Double, _ body: @differentiable(reverse) (Double) throws -> Double) throws -> Double {
    try body(x) * 2
}
