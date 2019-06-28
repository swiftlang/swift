// RUN: %target-swift-frontend -typecheck -verify %s

// Test top-level functions.

func linearFunc(_ x: Float) -> Float {
  return x
}

@transposing(linearFunc, wrt: 0) // ok
func transposingLinearFunc(x: Float) -> Float {
  return x
}

func twoParams(_ x: Float, _ y: Double) -> Double {
    return Double(x) + y
}

@transposing(twoParams, wrt: (0, 1)) // ok
func twoParamsT1(_ t: Double) ->  (Float, Double) {
    return (Float(t), t)
}

@transposing(twoParams, wrt: 0) // ok
func twoParamsT2(_ t: Double, _ y: Double) ->  Float {
    return Float(t + y)
}

@transposing(twoParams, wrt: 1) // ok
func twoParamsT3(_ x: Float, _ t: Double) ->  Double {
    return Double(x) + t
}

func generic<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

@transposing(generic, wrt: 0) // ok
func genericT<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

func withInt(x: Float, y: Int) -> Float {
  if y >= 0 {
    return x
  } else {
    return x
  }
}

@transposing(withInt, wrt: 0) // ok
func withIntT(x: Int, t: Float) -> Float {
  return t
}

func missingDiffSelfRequirement<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

// expected-error @+1 {{can only differentiate with respect to parameters that conform to 'Differentiable' and where 'T == T.TangentVector'}}
@transposing(missingDiffSelfRequirement, wrt: 0)
func missingDiffSelfRequirementT<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

// TODO: error should be "can only differentiate with respect to parameters that conform to 'Differentiable' and where 'T == T.TangentVector'"
// but currently there is an assertion failure.
/*func missingSelfRequirement<T: Differentiable>(x: T) 
  -> T where T.TangentVector == T {
  return x
}

@transposing(missingSelfRequirement, wrt: 0)
func missingSelfRequirementT<T: Differentiable>(x: T) -> T {
  return x
}*/

func differentGenericConstraint<T: Differentiable & BinaryFloatingPoint>(x: T)
  -> T where T == T.TangentVector {
  return x
}

// expected-error @+2 {{type 'T' does not conform to protocol 'BinaryFloatingPoint'}}
// expected-error @+1 {{could not find function 'differentGenericConstraint' with expected type '<T where T : Differentiable, T == T.TangentVector> (T) -> T'}}
@transposing(differentGenericConstraint, wrt: 0)
func differentGenericConstraintT<T: Differentiable>(x: T) 
  -> T where T == T.TangentVector {
  return x
}

