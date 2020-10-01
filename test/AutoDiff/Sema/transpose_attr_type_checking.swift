// RUN: %target-swift-frontend-typecheck -verify %s
// RUN: %target-swift-frontend-typecheck -enable-testing -verify %s

import _Differentiation

// ~~~~~~~~~~~~~ Test top-level functions. ~~~~~~~~~~~~~

func linearFunc(_ x: Float) -> Float {
  return x
}

@transpose(of: linearFunc, wrt: 0)
func linearFuncTranspose(x: Float) -> Float {
  return x
}

func twoParams(_ x: Float, _ y: Double) -> Double {
  return Double(x) + y
}

@transpose(of: twoParams, wrt: 0)
func twoParamsT1(_ y: Double, _ t: Double) -> Float {
  return Float(t + y)
}

@transpose(of: twoParams, wrt: 1)
func twoParamsT2(_ x: Float, _ t: Double) -> Double {
  return Double(x) + t
}

@transpose(of: twoParams, wrt: (0, 1))
func twoParamsT3(_ t: Double) -> (Float, Double) {
  return (Float(t), t)
}

func threeParams(_ x: Float, _ y: Double, _ z: Float) -> Double {
  return Double(x) + y
}

@transpose(of: threeParams, wrt: 0)
func threeParamsT1(_ y: Double, _ z: Float, _ t: Double) -> Float {
  return Float(t + y) + z
}

@transpose(of: threeParams, wrt: 1)
func threeParamsT2(_ x: Float, _ z: Float, _ t: Double) -> Double {
  return Double(x + z) + t
}

@transpose(of: threeParams, wrt: 2)
func threeParamsT3(_ x: Float, _ y: Double, _ t: Double) -> Float {
  return Float(y + t) + x
}

@transpose(of: threeParams, wrt: (0, 1))
func threeParamsT4(_ z: Float, _ t: Double) -> (Float, Double) {
  return (z + Float(t), Double(z) + t)
}

@transpose(of: threeParams, wrt: (0, 2))
func threeParamsT5(_ y: Double, _ t: Double) -> (Float, Float) {
  let ret = Float(y + t)
  return (ret, ret)
}

@transpose(of: threeParams, wrt: (0, 1, 2))
func threeParamsT5(_ t: Double) -> (Float, Double, Float) {
  let ret = Float(t)
  return (ret, t, ret)
}

// Generics
func generic<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

@transpose(of: generic, wrt: 0)
func genericT<T: Differentiable>(x: T) -> T where T == T.TangentVector {
  return x
}

func genericThreeParam<
  T: Differentiable & BinaryFloatingPoint,
  U: Differentiable & BinaryFloatingPoint,
  V: Differentiable & BinaryFloatingPoint>(
  t: T, u: U, v: V
) -> T where T == T.TangentVector,
             U == U.TangentVector,
             V == V.TangentVector {
  return t
}

@transpose(of: genericThreeParam, wrt: 1)
func genericThreeParamT2<
  T: Differentiable & BinaryFloatingPoint,
  U: Differentiable & BinaryFloatingPoint,
  V: Differentiable & BinaryFloatingPoint>(
  t: T, v: V, s: T
) -> U where T == T.TangentVector,
             U == U.TangentVector,
             V == V.TangentVector {
  return U(1)
}

@transpose(of: genericThreeParam, wrt: (0, 1, 2))
func genericThreeParamT2<
  T: Differentiable & BinaryFloatingPoint,
  U: Differentiable & BinaryFloatingPoint,
  V: Differentiable & BinaryFloatingPoint>(
  t: T
) -> (T, U, V) where T == T.TangentVector,
                     U == U.TangentVector,
                     V == V.TangentVector {
  return (T(1), U(1), V(1))
}

func genericOneParamFloatOneParam<T: Differentiable & BinaryFloatingPoint>(
  t: T, f: Float
) -> T where T == T.TangentVector {
  return T(f)
}

@transpose(of: genericOneParamFloatOneParam, wrt: 0)
func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>(
  f: Float, t: T
) -> T where T == T.TangentVector {
  return t
}

@transpose(of: genericOneParamFloatOneParam, wrt: 1)
func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>(
  t1: T, t2: T
) -> Float where T == T.TangentVector {
  return 1
}

@transpose(of: genericOneParamFloatOneParam, wrt: (0, 1))
func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>(
  t: T
) -> (T, Float) where T == T.TangentVector {
  return (T(1), 1)
}

func withInt(x: Float, y: Int) -> Float {
  if y >= 0 {
    return x
  } else {
    return x
  }
}

@transpose(of: withInt, wrt: 0)
func withIntT(x: Int, t: Float) -> Float {
  return t
}

func missingDiffSelfRequirement<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

// expected-error @+1 {{cannot transpose with respect to original result 'T' that does not conform to 'Differentiable' and satisfy 'T == T.TangentVector'}}
@transpose(of: missingDiffSelfRequirement, wrt: 0)
func missingDiffSelfRequirementT<T: AdditiveArithmetic>(x: T) -> T {
  return x
}

func missingSelfRequirement<T: Differentiable>(x: T)
  -> T where T.TangentVector == T {
  return x
}

// expected-error @+1 {{cannot transpose with respect to original result 'T' that does not conform to 'Differentiable' and satisfy 'T == T.TangentVector'}}
@transpose(of: missingSelfRequirement, wrt: 0)
func missingSelfRequirementT<T: Differentiable>(x: T) -> T {
  return x
}

// expected-note @+1 {{candidate global function does not have type equal to or less constrained than '<T where T : Differentiable, T == T.TangentVector> (T) -> T'}}
func differentGenericConstraint<T: Differentiable & BinaryFloatingPoint>(x: T)
-> T where T == T.TangentVector {
  return x
}

// expected-error @+1 {{referenced declaration 'differentGenericConstraint' could not be resolved}}
@transpose(of: differentGenericConstraint, wrt: 0)
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

// expected-error @+1 {{cannot transpose with respect to original parameter 'Int' that does not conform to 'Differentiable' and satisfy 'Int == Int.TangentVector'}}
@transpose(of: transposingInt, wrt: 1)
func transposingIntT1(x: Float, t: Float) -> Int {
  return Int(x)
}

@transpose(of: transposingInt, wrt: 0)
func tangentNotLast(y: Int, t: Float) -> Float {
  return t
}

// ~~~~~~~~~~~~~ Test methods. ~~~~~~~~~~~~~

// // Method no parameters.
extension Float {
  func getDouble() -> Double {
      return Double(self)
  }

  @transpose(of: Float.getDouble, wrt: self)
  static func structTranspose(v: Double) -> Float {
    return Float(v)
  }
}

// Method with one parameter.
extension Float {
  func adding(_ double: Double) -> Float {
    return self + Float(double)
  }

  @transpose(of: Float.adding, wrt: 0)
  func addingT1(t: Float) -> Double {
    return Double(self + t)
  }

  @transpose(of: Float.adding, wrt: self)
  static func addingT2(_ double: Double, t: Float) -> Float {
    return Float(double) + t
  }

  @transpose(of: Float.adding, wrt: (self, 0))
  static func addingT3(t: Float) -> (Float, Double) {
    return (t, Double(t))
  }
}

// Different self type/result type.
extension Int {
  func myAdding(_ double: Double) -> Float {
    return Float(double)
  }

  @transpose(of: Int.myAdding, wrt: 0)
  func addingT3(t: Float) -> Double {
    return Double(t)
  }

  // expected-error @+1 {{cannot transpose with respect to original parameter 'Int' that does not conform to 'Differentiable' and satisfy 'Int == Int.TangentVector'}}
  @transpose(of: Int.myAdding, wrt: (self, 0))
  static func addingT3(v: Float) -> (Int, Double) {
    return (Int(v), Double(v))
  }
}

// Static methods.
struct A : Differentiable & AdditiveArithmetic {
  typealias TangentVector = A
  var x: Double

  static prefix func -(a: A) -> A {
    return A(x: -a.x)
  }

  @transpose(of: -, wrt: 0)
  static func transposeNegate(t: A) -> A {
    return A(x: -t.x)
  }

  static prefix func +(a: A) -> A {
    return a
  }

  // TODO(TF-1065): Consider disallowing qualified operator names.
  @transpose(of: A.+, wrt: 0)
  static func transposeIdQualified(t: A) -> A {
    return t
  }
}

extension Float {
  static func myMultiply(lhs: Float, rhs: Float) -> Float {
    return lhs * rhs
  }

  @transpose(of: Float.myMultiply, wrt: 0)
  @transpose(of: Float.myMultiply, wrt: 1)
  static func myMultiplyT(param: Float, v: Float) -> Float {
    return param + v
  }

  static func threeParamsStatic(_ x: Float, _ y: Double, _ z: Float) -> Double {
    return Double(x + z) + y
  }

  @transpose(of: Float.threeParamsStatic, wrt: (0, 1, 2))
  static func threeParamsT12(v: Double) -> (x: Float, y: Double, z: Float) {
    return (Float(v), v, Float(v))
  }

  @transpose(of: Float.threeParamsStatic, wrt: (0, 2))
  static func threeParamsT12(_ y: Double, v: Double) -> (x: Float, z: Float) {
    let ret = Float(y + v)
    return (ret, ret)
  }

  @transpose(of: Float.threeParamsStatic, wrt: 1)
  static func threeParamsT12(_ x: Float, _ z: Float, v: Double) -> Double {
    return v + Double(x + z)
  }
}

// Method with 3 parameters.
extension Float {
  func threeParams(_ x: Float, _ y: Double, _ z: Float) -> Double {
    return Double(self + x + z) + y
  }

  @transpose(of: Float.threeParams, wrt: 0)
  func threeParamsT1(_ y: Double, _ z: Float, t: Double) -> Float {
    return self + Float(t + y) + z
  }

  @transpose(of: Float.threeParams, wrt: 1)
  func threeParamsT2(_ x: Float, _ z: Float, t: Double) -> Double {
    return t + Double(x + z + self)
  }

  @transpose(of: Float.threeParams, wrt: 2)
  func threeParamsT3(_ x: Float, _ y: Double, t: Double) -> Float {
    return x + Float(y + t) + self
  }

  @transpose(of: Float.threeParams, wrt: (0, 1))
  func threeParamsT4(_ z: Float, t: Double) -> (x: Float, y: Double) {
    return (Float(t) + z + self, t + Double(z + self))
  }

  @transpose(of: Float.threeParams, wrt: (0, 2))
  func threeParamsT5(_ y: Double, t: Double) -> (x: Float, z: Float) {
    let ret = Float(y + t) + self
    return (ret, ret)
  }

  @transpose(of: Float.threeParams, wrt: (0, 1, 2))
  func threeParamsT6(t: Double) -> (x: Float, y: Double, z: Float) {
    return (Float(t) + self, t + Double(self), Float(t) + self)
  }

  @transpose(of: Float.threeParams, wrt: self)
  static func threeParamsT6(_ x: Float, _ y: Double, _ z: Float, t: Double) -> Float {
    return x + z + Float(y + t)
  }

  @transpose(of: Float.threeParams, wrt: (self, 0))
  static func threeParamsT7(_ y: Double, _ z: Float, t: Double) -> (self: Float, x: Float) {
    let ret = Float(y + t) + z
    return (ret, ret)
  }

  @transpose(of: Float.threeParams, wrt: (self, 1))
  static func threeParamsT7(_ x: Float, _ z: Float, t: Double) -> (self: Float, y: Double) {
    return (x + z + Float(t), t + Double(x + z))
  }

  @transpose(of: Float.threeParams, wrt: (self, 2))
  static func threeParamsT9(_ x: Float, _ y: Double, t: Double) -> (self: Float, z: Float) {
    let ret = Float(y + t) + x
    return (ret, ret)
  }

  @transpose(of: Float.threeParams, wrt: (self, 0, 1))
  static func threeParamsT10(_ z: Float, t: Double) -> (self: Float, x: Float, y: Double) {
    let ret = Float(t) + z
    return (ret, ret, Double(ret))
  }

  @transpose(of: Float.threeParams, wrt: (self, 0, 2))
  static func threeParamsT11(_ y: Double, t: Double) -> (self: Float, x: Float, z: Float) {
    let ret = Float(t + y)
    return (ret, ret, ret)
  }

  @transpose(of: Float.threeParams, wrt: (self, 0, 1, 2))
  static func threeParamsT12(t: Double) -> (self: Float, x: Float, y: Double, z: Float) {
    return (Float(t), Float(t), t, Float(t))
  }
}

// Nested struct
struct level1 {
  struct level2: Differentiable & AdditiveArithmetic {
    static var zero: Self { Self() }
    static func + (_: Self, _: Self) -> Self { Self() }
    static func - (_: Self, _: Self) -> Self { Self() }
    typealias TangentVector = Self
    mutating func move(along: TangentVector) {}
    func foo(x: Float) -> Float {
      return x
    }
  }
  struct level2_nondiff {
    func foo(x: Float) -> Float {
      return x
    }
  }
}

extension level1.level2 {
  @transpose(of: foo, wrt: 0)
  func trans(t: Float) -> Float {
    return t
  }

  @transpose(of: foo, wrt: (self, 0))
  static func trans(t: Float) -> (self: level1.level2, x: Float) {
    return (level1.level2(), t)
  }
}

extension level1.level2_nondiff {
  // expected-error @+1 {{cannot transpose with respect to original parameter 'level1.level2_nondiff' that does not conform to 'Differentiable' and satisfy 'level1.level2_nondiff == level1.level2_nondiff.TangentVector'}}
  @transpose(of: level1.level2_nondiff.foo, wrt: (self, 0))
  static func trans(t: Float) -> (self: level1.level2_nondiff, x: Float) {
    return (level1.level2_nondiff(), t)
  }
}

// Generics
extension Float {
  func genericOneParamFloatOneParam<T: Differentiable & BinaryFloatingPoint>(
    x: T, y: Float
  ) -> Float where T == T.TangentVector {
    return y + Float(x)
  }

  @transpose(of: Float.genericOneParamFloatOneParam, wrt: 0)
  func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>(
    y: Float, t: Float
  ) -> T where T == T.TangentVector {
    return T(y + t)
  }

  @transpose(of: Float.genericOneParamFloatOneParam, wrt: (0, 1))
  func genericOneParamFloatOneParamT2<T: Differentiable & BinaryFloatingPoint>(
    t: Float
  ) -> (x: T, y: Float) where T == T.TangentVector {
    return (T(t), t)
  }

  @transpose(of: Float.genericOneParamFloatOneParam, wrt: (self, 1))
  static func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>(
    x: T,
    t: Float
  ) -> (self: Float, y: Float) where T == T.TangentVector {
    return (Float(x) + t, Float(x) + t)
  }

  @transpose(of: Float.genericOneParamFloatOneParam, wrt: (self, 0, 1))
  static func genericOneParamFloatOneParamT1<T: Differentiable & BinaryFloatingPoint>(
    t: Float
  ) -> (self: Float, x: T, y: Float) where T == T.TangentVector {
    return (t, T(t), t)
  }
}

// Test non-`func` original declarations.

// expected-note @+1 {{candidate initializer does not have type equal to or less constrained than '<T where T : Differentiable, T == T.TangentVector> (Struct<T>) -> (Float) -> Struct<T>'}}
struct Struct<T> {}
extension Struct: Equatable where T: Equatable {}
extension Struct: Differentiable & AdditiveArithmetic
where T: Differentiable & AdditiveArithmetic {
  static var zero: Self { Self() }
  static func + (_: Self, _: Self) -> Self { Self() }
  static func - (_: Self, _: Self) -> Self { Self() }
  typealias TangentVector = Self
  mutating func move(along: TangentVector) {}
}

// Test computed properties.
extension Struct {
  var computedProperty: Struct { self }
}
extension Struct where T: Differentiable & AdditiveArithmetic {
  @transpose(of: computedProperty, wrt: self)
  static func transposeProperty(t: Self) -> Self {
    t
  }
}

// Test initializers.
extension Struct {
  // expected-note @+1 {{candidate initializer does not have type equal to or less constrained than '<T where T : Differentiable, T == T.TangentVector> (Struct<T>) -> (Float) -> Struct<T>'}}
  init(_ x: Float) {}
  // expected-note @+1 {{candidate initializer does not have type equal to or less constrained than '<T where T : Differentiable, T == T.TangentVector> (Struct<T>) -> (Float) -> Struct<T>'}}
  init(_ x: T, y: Float) {}
}

extension Struct where T: Differentiable, T == T.TangentVector {
  @transpose(of: init, wrt: 0)
  static func vjpInitX(_ x: Self) -> Float {
    fatalError()
  }

  @transpose(of: init(_:y:), wrt: (0, 1))
  static func vjpInitXY(_ x: Self) -> (T, Float) {
    fatalError()
  }

  // Test instance transpose for static original intializer.
  // TODO(TF-1015): Add improved instance/static member mismatch error.
  // expected-error @+1 {{referenced declaration 'init' could not be resolved}}
  @transpose(of: init, wrt: 0)
  func vjpInitStaticMismatch(_ x: Self) -> Float {
    fatalError()
  }
}

// Test subscripts.
extension Struct {
  subscript() -> Self {
    get { self }
    set {}
  }
  subscript(float float: Float) -> Self { self }
  subscript<U: Differentiable>(x: U) -> Self { self }
}

extension Struct where T: Differentiable & AdditiveArithmetic {
  @transpose(of: subscript, wrt: self)
  static func vjpSubscript(t: Struct) -> Struct {
    t
  }

  @transpose(of: subscript(float:), wrt: self)
  static func vjpSubscriptLabelled(float: Float, t: Struct) -> Struct {
    t
  }

  @transpose(of: subscript(_:), wrt: self)
  static func vjpSubscriptGeneric<U: Differentiable>(x: U, t: Struct) -> Struct {
    t
  }
}

// Check that `@transpose` attribute rejects stored property original declarations.

struct StoredProperty: Differentiable & AdditiveArithmetic {
  // expected-note @+1 {{candidate getter does not have expected type '(StoredProperty) -> () -> StoredProperty'}}
  var stored: Float
  typealias TangentVector = StoredProperty
  static var zero: StoredProperty { StoredProperty(stored: 0) }
  static func + (_: StoredProperty, _: StoredProperty) -> StoredProperty {
    StoredProperty(stored: 0)
  }
  static func - (_: StoredProperty, _: StoredProperty) -> StoredProperty {
    StoredProperty(stored: 0)
  }

  // Note: `@transpose` support for instance members is currently too limited
  // to properly register a transpose for a non-`Self`-typed member.

  // expected-error @+1 {{referenced declaration 'stored' could not be resolved}}
  @transpose(of: stored, wrt: self)
  static func vjpStored(v: Self) -> Self {
    fatalError()
  }
}

// Check that the self type of the method and the result type are the same when
// transposing WRT self. Needed to make sure they are defined within the same
// context.

extension Float {
  func convertToDouble() -> Double {
    Double(self)
  }

  // Ok
  @transpose(of: convertToDouble, wrt: self)
  static func t1(t: Double) -> Float {
    Float(t)
  }
}
extension Double {
  // expected-error @+2 {{the transpose of an instance method must be a 'static' method in the same type when 'self' is a linearity parameter}}
  // expected-note @+1 {{the transpose is declared in 'Double' but the original function is declared in 'Float'}}
  @transpose(of: Float.convertToDouble, wrt: self)
  static func t1(t: Double) -> Float {
    Float(t)
  }
}
