// RUN: %target-typecheck-verify-swift

public struct Complex {
  public var real = 0.0, imag = 0.0
  public func magnitude() -> Double {
    return real * real + imag * imag
  }
  public init() {}
  public init(real: Double, imag: Double) {
    self.real = real
    self.imag = imag
  }
}

public func * (lhs: Complex, rhs: Complex) -> Complex {
  return Complex(real: lhs.real * rhs.real - lhs.imag * rhs.imag,
                 imag: lhs.real * rhs.imag + lhs.imag * rhs.real)
}
public func + (lhs: Complex, rhs: Complex) -> Complex {
  return Complex(real: lhs.real + rhs.real, imag: lhs.imag + rhs.imag)
}
