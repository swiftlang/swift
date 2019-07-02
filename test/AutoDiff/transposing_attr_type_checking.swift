// RUN: %target-swift-frontend -typecheck -verify %s

// ~~~~~~~~~~~~~ Test top-level functions. ~~~~~~~~~~~~~

func linearFunc(_ x: Float) -> Float {
  return x
}

@transposing(linearFunc, wrt: 0) 
func transposingLinearFunc(x: Float) -> Float {
  return x
}

func twoParams(_ x: Float, _ y: Double) -> Double {
  return Double(x) + y
}

@transposing(twoParams, wrt: 0) 
func twoParamsT1(_ y: Double, _ t: Double) ->  Float {
  return Float(t + y)
}

@transposing(twoParams, wrt: 1) 
func twoParamsT2(_ x: Float, _ t: Double) ->  Double {
  return Double(x) + t
}

@transposing(twoParams, wrt: (0, 1)) 
func twoParamsT3(_ t: Double) ->  (Float, Double) {
  return (Float(t), t)
}

func threeParams(_ x: Float, _ y: Double, _ z: Float) -> Double {
  return Double(x) + y
}

@transposing(threeParams, wrt: 0) 
func threeParamsT1(_ y: Double, _ z: Float, _ t: Double) -> Float {
  return Float(t + y) + z
}

@transposing(threeParams, wrt: 1) 
func threeParamsT2(_ x: Float, _ z: Float, _ t: Double) -> Double {
  return Double(x + z) + t
}

@transposing(threeParams, wrt: 2) 
func threeParamsT3(_ x: Float, _ y: Double, _ t: Double) -> Float {
  return Float(y + t) + x
}

@transposing(threeParams, wrt: (0, 1)) 
func threeParamsT4(_ z: Float, _ t: Double) -> (Float, Double) {
  return (z + Float(t), Double(z) + t)
}

@transposing(threeParams, wrt: (0, 2))
func threeParamsT5(_ y: Double, _ t: Double) -> (Float, Float) {
  let ret = Float(y + t)
  return (ret, ret)
}

@transposing(threeParams, wrt: (0, 1, 2))
func threeParamsT5(_ t: Double) -> (Float, Double, Float) {
  let ret = Float(t)
  return (ret, t, ret)
}

// Generics
func generic<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

@transposing(generic, wrt: 0)
func genericT<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

func genericThreeParam
  <T: Differentiable & BinaryFloatingPoint, U: Differentiable & BinaryFloatingPoint, V: Differentiable & BinaryFloatingPoint>
  (t: T, u: U, v: V) -> T 
  where T == T.TangentVector,
        U == U.TangentVector,
        V == V.TangentVector {
  return t
}

@transposing(genericThreeParam, wrt: 1)
func genericThreeParamT2
  <T: Differentiable & BinaryFloatingPoint, U: Differentiable & BinaryFloatingPoint, V: Differentiable & BinaryFloatingPoint>
  (t: T, v: V, s: T) -> U
  where T == T.TangentVector,
        U == U.TangentVector,
        V == V.TangentVector {
  return U(1)
}

@transposing(genericThreeParam, wrt: (0, 1, 2))
func genericThreeParamT2
  <T: Differentiable & BinaryFloatingPoint, U: Differentiable & BinaryFloatingPoint, V: Differentiable & BinaryFloatingPoint>
  (t: T) -> (T, U, V)
  where T == T.TangentVector,
        U == U.TangentVector,
        V == V.TangentVector {
  return (T(1), U(1), V(1))
}

func genericOneParamFloatOneParam<T: Differentiable & BinaryFloatingPoint>
  (t: T, f: Float) -> T where T == T.TangentVector {
  return T(f)
}

@transposing(genericOneParamFloatOneParam, wrt: 0)
func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>
  (f: Float, t: T) -> T where T == T.TangentVector {
  return t
}

@transposing(genericOneParamFloatOneParam, wrt: 1)
func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>
  (t1: T, t2: T) -> Float where T == T.TangentVector {
  return 1
}

@transposing(genericOneParamFloatOneParam, wrt: (0, 1))
func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>
  (t: T) -> (T, Float) where T == T.TangentVector {
  return (T(1), 1)
}

func withInt(x: Float, y: Int) -> Float {
  if y >= 0 {
    return x
  } else {
    return x
  }
}

@transposing(withInt, wrt: 0)
func withIntT(x: Int, t: Float) -> Float {
  return t
}

func missingDiffSelfRequirement<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

// expected-error @+1 {{'@transposing' attribute requires original function result to conform to 'Differentiable'}}
@transposing(missingDiffSelfRequirement, wrt: 0)
func missingDiffSelfRequirementT<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

// TODO: error should be "can only transpose with respect to parameters that conform to 'Differentiable' and where 'Int == Int.TangentVector'"
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

func transposingInt(x: Float, y: Int) -> Float {
  if y >= 0 {
    return x
  } else {
    return x
  }
}

// expected-error @+1 {{can only transpose with respect to parameters that conform to 'Differentiable' and where 'Int == Int.TangentVector'}}
@transposing(transposingInt, wrt: 1) 
func transposingIntT1(x: Float, t: Float) -> Int {
  return Int(x)
}


// expected-error @+1 {{'@transposing' attribute requires original function result to conform to 'Differentiable'}}
@transposing(transposingInt, wrt: 0)
func tangentNotLast(t: Float, y: Int) -> Float {
  return t
}

// ~~~~~~~~~~~~~ Test methods. ~~~~~~~~~~~~~

// // Method no parameters.
extension Float {
  func getDouble() -> Double {
      return Double(self)
  }
}

extension Double {
  @transposing(Float.getDouble, wrt: self)
  func structTranspose() -> Float {
    return Float(self)
  }
}

// Method with one parameter.
extension Float {
  func adding(_ double: Double) -> Float {
    return self + Float(double)
  }

  @transposing(Float.adding, wrt: 0)
  func addingT1(t: Float) -> Double {
    return Double(self + t)
  }

  @transposing(Float.adding, wrt: self)
  func addingT2(_ double: Double) -> Float {
    return self + Float(double)
  }
    
  @transposing(Float.adding, wrt: (self, 0))
  func addingT3() -> (Float, Double) {
    return (self, Double(self))
  }
}

// Different self type/result type.
extension Int {
  func myAdding(_ double: Double) -> Float {
    return Float(double)
  }
}

extension Float {
  @transposing(Int.myAdding, wrt: 0)
  func addingT3(t: Int) -> Double {
    return Double(self)
  }

  // TODO: throw an error due to Int not being differentiable.
  // @transposing(Int.myAdding, wrt: (self, 0))
  // func addingT3() -> (Int, Double) {
  //   return (Int(self), Double(self))
  // }
}

// Static method.
struct A : Differentiable & AdditiveArithmetic {
  typealias TangentVector = A
  var x: Double
  
  static prefix func -(a: A) -> A {
    return A(x: -a.x)
  }

  @transposing(A.-, wrt: 0)
  func negationT(a: A.Type) -> A {
    return A(x: 1)
  }
}

// Method with 3 parameters.
extension Float {
  func threeParams(_ x: Float, _ y: Double, _ z: Float) -> Double {
    return Double(self + x + z) + y
  }
}

extension Double {
  @transposing(Float.threeParams, wrt: 0) 
  func threeParamsT1(_ s: Float, _ y: Double, _ z: Float) -> Float {
    return Float(self + y) + s + z
  }

  @transposing(Float.threeParams, wrt: 1) 
  func threeParamsT2(_ s: Float, _ x: Float, _ y: Float) -> Double {
    return self + Double(x + s + y)
  }

  @transposing(Float.threeParams, wrt: 2) 
  func threeParamsT3(_ s: Float, _ x: Float, _ y: Double) -> Float {
    return s + x + Float(self + y)
  }

  @transposing(Float.threeParams, wrt: (0, 1)) 
  func threeParamsT4(_ s: Float, _ z: Float) -> (Float, Double) {
    return (Float(self) + s + z, self + Double(s + z))
  }

  @transposing(Float.threeParams, wrt: (0, 2)) 
  func threeParamsT5(_ s: Float, _ y: Double) -> (Float, Float) {
    let ret = Float(self + y) + s
    return (ret, ret)
  }

  @transposing(Float.threeParams, wrt: (0, 1, 2))
  func threeParamsT6(s: Float) -> (Float, Double, Float) {
    return (s + Float(self), Double(s) + self, s + Float(self))
  }

  @transposing(Float.threeParams, wrt: self) 
  func threeParamsT6(_ x: Float, _ y: Double, _ z: Float) -> Float {
    return Float(self + y) + x
  }

  @transposing(Float.threeParams, wrt: (self, 0)) 
  func threeParamsT7(_ y: Double, _ z: Float) -> (Float, Float) {
    let ret = Float(self) + Float(y) + z
    return (ret, ret)
  }

  @transposing(Float.threeParams, wrt: (self, 1)) 
  func threeParamsT7(_ x: Float, _ z: Float) -> (Float, Double) {
    return (Float(self) + x + z, self + Double(x + z))
  }

  @transposing(Float.threeParams, wrt: (self, 2)) 
  func threeParamsT9(_ x: Float, _ y: Double) -> (Float, Float) {
    let ret = Float(self + y) + x
    return (ret, ret)
  }

  @transposing(Float.threeParams, wrt: (self, 0, 1)) 
  func threeParamsT10(_ z: Float) -> (Float, Float, Double) {
    let retF = Float(self) + z
    return (retF, retF, self + Double(z))
  }

  @transposing(Float.threeParams, wrt: (self, 0, 2)) 
  func threeParamsT11(_ y: Double) -> (Float, Float, Float) {
    let ret = Float(self + y)
    return (ret, ret, ret)
  }

  @transposing(Float.threeParams, wrt: (self, 0, 1, 2)) 
  func threeParamsT12() -> (Float, Float, Double, Float) {
    return (Float(self), Float(self), self, Float(self))
  }
}

// Nested struct
struct level1 {
  struct level2 {
    func foo(x: Float) -> Float {
      return x
    }
  }
}

extension Float {
  @transposing(level1.level2.foo, wrt: 0)
  func t(x: level1.level2) -> Float {
    return self
  }

  @transposing(level1.level2.foo, wrt: (self, 0))
  func t() -> (level1.level2, Float) {
    return (level1.level2(), self)
  }
}

// Generics
extension Float {
  func genericOneParamFloatOneParam<T: Differentiable & BinaryFloatingPoint>
    (t: T, f: Float) -> Float where T == T.TangentVector {
    return f
  }

  @transposing(Float.genericOneParamFloatOneParam, wrt: 0)
  func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>
    (f1: Float, f2: Float) -> T where T == T.TangentVector {
    return T(1)
  }

  @transposing(Float.genericOneParamFloatOneParam, wrt: (0, 1))
  func genericOneParamFloatOneParamT2<T: Differentiable & BinaryFloatingPoint>
    (f1: Float) -> (T, Float) where T == T.TangentVector {
    return (T(1), 1)
  }

  @transposing(Float.genericOneParamFloatOneParam, wrt: (self, 1))
  func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>
    (t: T) -> (Float, Float) where T == T.TangentVector {
    return (1, 1)
  }

  @transposing(Float.genericOneParamFloatOneParam, wrt: (self, 0, 1))
  func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>
    () -> (Float, T, Float) where T == T.TangentVector {
    return (1, T(1), 1)
  }
}