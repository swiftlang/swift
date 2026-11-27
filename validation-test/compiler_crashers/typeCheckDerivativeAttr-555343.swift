// {"kind":"typecheck","signature":"typeCheckDerivativeAttr(swift::DerivativeAttr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypeChecker::checkDeclAttributes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
func f(_ x: Float) -> Float {
}
@derivative(of: f)
func g(_ x: Float) -> sending (value: Float, pullback: @isolated(any) (Float) -> Float) {
}
