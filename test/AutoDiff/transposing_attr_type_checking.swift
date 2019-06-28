// RUN: %target-swift-frontend -typecheck -verify %s

// Test top-level functions.

func linearFunc(_ x: Float) -> Float {
  return x
}

@transposing(linearFunc, wrt: 0) // ok
func transposingLinearFunc(x: Float) -> Float {
  return x
}

func foo1(_ x: Float, _ y: Double) -> Double {
    return Double(x) + y
}
@transposing(foo1, wrt: (0, 1)) // ok
func bar1(_ t: Double) ->  (Float, Double) {
    return (Float(t), t)
}

func foo2<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

@transposing(foo2, wrt: 0)
func bar2<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

func foo3<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

// expected-error @+1 {{can only differentiate with respect to parameters that conform to 'Differentiable' and where 'T == T.TangentVector'}}
@transposing(foo3, wrt: 0)
func bar3<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

func foo4<T: Differentiable>(x: T) -> T {
  return x
}

// expected-error @+1 {{can only differentiate with respect to parameters that conform to 'Differentiable' and where 'T == T.TangentVector'}}
@transposing(foo4, wrt: 0)
func bar4<T: Differentiable>(x: T) -> T {
  return x
}

func foo5<T: Differentiable & BinaryFloatingPoint>(x: T) -> T 
where T == T.TangentVector {
  return x
}

// expected-error @+2 {{type 'T' does not conform to protocol 'BinaryFloatingPoint'}}
// expected-error @+1 {{could not find function 'foo5' with expected type '<T where T : Differentiable, T == T.TangentVector> (T) -> T'}}
@transposing(foo5, wrt: 0)
func bar5<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}
