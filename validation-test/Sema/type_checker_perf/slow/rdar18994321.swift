// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

precedencegroup ExponentiationPrecedence {
  associativity: right
  higherThan: BitwiseShiftPrecedence
}

infix operator ** : ExponentiationPrecedence

func ** (num: Float, power: Float) -> Float {
  fatalError()
}

func test(f: Float) {
  let v1: Float = 1.1
  let v2 = 2.2
  let v3 = 3.3

  // NOTE: This is using mixed types, and would result in a type checking error if it completed.
  let _ = v2*7.1**(1.97276*(1-v1/f)-7.02202*v3+1.70373*1e-3*(1.0-10**(-2.29692*(f/v1-1.0)))+0.32273*1e-3*(10**(3.76977*(1.0-v1/f))-1)-2.2195923)
  // expected-error@-1 {{the compiler is unable to type-check this expression in reasonable time; try breaking up the expression into distinct sub-expressions}}
}
