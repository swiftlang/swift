// RUN: %swift -verify -parse %s

struct Complex {
  var real = 0.0, imag = 0.0
  func magnitude() -> Double {
    return real * real + imag * imag
  }
}

func * (lhs: Complex, rhs: Complex) -> Complex {
  return Complex(real: lhs.real * rhs.real - lhs.imag * rhs.imag,
                 imag: lhs.real * rhs.imag + lhs.imag * rhs.real)
}
func + (lhs: Complex, rhs: Complex) -> Complex {
  return Complex(real: lhs.real + rhs.real, imag: lhs.imag + rhs.imag)
}
