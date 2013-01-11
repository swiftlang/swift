// XXX FIXME -- this causes the constraint checker to take forever

/* ... matching brace at EOF

struct Complex<T : Numeric> {
  var real : T
  var imag : T

/*
  func replPrint() {
    print("real: \(real) imag: \(imag)")
  }
*/
}

func - <T : Numeric>(a : Complex<T>) -> Complex<T> {
  typealias ComplexT = Complex<T>
  return ComplexT(T.zero() - a.real, T.zero() - a.imag)
}
func + <T : Numeric>(a : Complex<T>) -> Complex<T> {
  return a
}

func [infix=160] == <T : Numeric>(lhs : Complex<T>, rhs : Complex<T>) -> Bool {
  return lhs.real == rhs.real && lhs.imag == rhs.imag
}
func [infix=160] != <T : Numeric>(lhs : Complex<T>, rhs : Complex<T>) -> Bool {
  return lhs.real != rhs.real || lhs.imag != rhs.imag
}

func [infix_left=190] + <T : Numeric>(lhs : Complex<T>, rhs : Complex<T>) -> Complex<T> {
  typealias ComplexT = Complex<T>
  return ComplexT(lhs.real + rhs.real, lhs.imag + rhs.imag)
}
func [infix_left=190] - <T : Numeric>(lhs : Complex<T>, rhs : Complex<T>) -> Complex<T> {
  typealias ComplexT = Complex<T>
  return ComplexT(lhs.real - rhs.real, lhs.imag - rhs.imag)
}
func [infix_left=200] * <T : Numeric>(lhs : Complex<T>, rhs : Complex<T>) -> Complex<T> {
  typealias ComplexT = Complex<T>
  return ComplexT(lhs.real * rhs.real - lhs.imag * rhs.imag,
                 lhs.real * rhs.imag + lhs.imag * rhs.real)
}
func [infix_left=200] / <T : Numeric>(lhs : Complex<T>, rhs : Complex<T>) -> Complex<T> {
  typealias ComplexT = Complex<T>
  return ComplexT((lhs.real * rhs.real + lhs.imag * rhs.imag) /
                 (rhs.real * rhs.real + rhs.imag * rhs.imag),
                 (lhs.imag * rhs.real - lhs.real * rhs.imag) /
                 (rhs.real * rhs.real + rhs.imag * rhs.imag))
}

/*

// Binary Remainder.
// The sign of the result matches the sign of the dividend.
// 1) This is consistent with '%' in C#, D, Java, and JavaScript
// 2) C99 requires this behavior for fmod*()
// 3) C++11 requires this behavior for std::fmod*()
func [asmname="fmodf",infix_left=200] % (lhs: Float32, rhs: Float32) -> Float32


// math
func [asmname="sinf"]    sin(x : Float32) -> Float32
func [asmname="cosf"]    cos(x : Float32) -> Float32
func [asmname="tanf"]    tan(x : Float32) -> Float32
func [asmname="atanf"]  atan(x : Float32) -> Float32
func [asmname="atan2f"] atan(y : Float32, x : Float32) -> Float32
func [asmname="sqrtf"]  sqrt(x : Float32) -> Float32

*/

*/
