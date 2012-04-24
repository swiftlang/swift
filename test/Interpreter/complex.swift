// RUN: %swift -I %S/.. %s -verify 

import swift

struct Complex {
  Real : Double,
  Imaginary : Double
  func magnitude() -> Double {
    return Real * Real + Imaginary * Imaginary
  }
}

func [infix_left=200] * (lhs : Complex, rhs : Complex) -> Complex {
  return Complex(lhs.Real * rhs.Real - lhs.Imaginary * rhs.Imaginary,
                 lhs.Real * rhs.Imaginary + lhs.Imaginary * rhs.Real)
}
func [infix_left=190] + (lhs: Complex, rhs: Complex) -> Complex {
  return Complex(lhs.Real + rhs.Real, lhs.Imaginary + rhs.Imaginary)
}
