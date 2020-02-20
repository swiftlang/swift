// RUN: %target-swift-frontend-typecheck -enable-experimental-differentiable-programming -verify %s
// REQUIRES: differentiable_programming

import _Differentiation

// Dummy `Differentiable`-conforming type.
struct DummyTangentVector: Differentiable & AdditiveArithmetic {
  static var zero: Self { Self() }
  static func + (_: Self, _: Self) -> Self { Self() }
  static func - (_: Self, _: Self) -> Self { Self() }
  typealias TangentVector = Self
}

@differentiable // expected-error {{'@differentiable' attribute cannot be applied to this declaration}}
let globalConst: Float = 1

@differentiable // expected-error {{'@differentiable' attribute cannot be applied to this declaration}}
var globalVar: Float = 1

func testLocalVariables() {
  // expected-error @+1 {{'_' has no parameters to differentiate with respect to}}
  @differentiable
  var getter: Float {
    return 1
  }

  // expected-error @+1 {{'_' has no parameters to differentiate with respect to}}
  @differentiable
  var getterSetter: Float {
    get { return 1 }
    set {}
  }
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: dfoo) // expected-error {{'@differentiable' attribute cannot be applied to this declaration}}
protocol P {}

@differentiable() // ok!
func no_jvp_or_vjp(_ x: Float) -> Float {
  return x * x
}

// Test duplicate `@differentiable` attributes.

@differentiable // expected-error {{duplicate '@differentiable' attribute with same parameters}}
@differentiable // expected-note {{other attribute declared here}}
func dupe_attributes(arg: Float) -> Float { return arg }

@differentiable(wrt: arg1)
@differentiable(wrt: arg2) // expected-error {{duplicate '@differentiable' attribute with same parameters}}
@differentiable(wrt: arg2) // expected-note {{other attribute declared here}}
func dupe_attributes(arg1: Float, arg2: Float) -> Float { return arg1 }

struct ComputedPropertyDupeAttributes<T: Differentiable>: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  var value: T

  @differentiable // expected-note {{other attribute declared here}}
  var computed1: T {
    @differentiable // expected-error {{duplicate '@differentiable' attribute with same parameters}}
    get { value }
    set { value = newValue }
  }

  // TODO(TF-482): Remove diagnostics when `@differentiable` attributes are
  // also uniqued based on generic requirements.
  @differentiable(where T == Float) // expected-error {{duplicate '@differentiable' attribute with same parameters}}
  @differentiable(where T == Double) // expected-note {{other attribute declared here}}
  var computed2: T {
    get { value }
    set { value = newValue }
  }
}

// Test TF-568.
protocol WrtOnlySelfProtocol: Differentiable {
  @differentiable
  var computedProperty: Float { get }

  @differentiable
  func method() -> Float
}

class Class: Differentiable {
  typealias TangentVector = DummyTangentVector
  func move(along _: TangentVector) {}
}
@differentiable(wrt: x)
func invalidDiffWrtClass(_ x: Class) -> Class {
  return x
}

protocol Proto {}
// expected-error @+1 {{can only differentiate with respect to parameters that conform to 'Differentiable', but 'Proto' does not conform to 'Differentiable'}}
@differentiable(wrt: x)
func invalidDiffWrtExistential(_ x: Proto) -> Proto {
  return x
}

// expected-error @+1 {{can only differentiate with respect to parameters that conform to 'Differentiable', but '@differentiable (Float) -> Float' does not conform to 'Differentiable'}}
@differentiable(wrt: fn)
func invalidDiffWrtFunction(_ fn: @differentiable(Float) -> Float) -> Float {
  return fn(.pi)
}

// expected-error @+1 {{'invalidDiffNoParams()' has no parameters to differentiate with respect to}}
@differentiable
func invalidDiffNoParams() -> Float {
  return 1
}

// expected-error @+1 {{cannot differentiate void function 'invalidDiffVoidResult(x:)'}}
@differentiable
func invalidDiffVoidResult(x: Float) {}

// Test static methods.
struct StaticMethod {
  // expected-error @+1 {{'invalidDiffNoParams()' has no parameters to differentiate with respect to}}
  @differentiable
  static func invalidDiffNoParams() -> Float {
    return 1
  }

  // expected-error @+1 {{cannot differentiate void function 'invalidDiffVoidResult(x:)'}}
  @differentiable
  static func invalidDiffVoidResult(x: Float) {}
}

// Test instance methods.
struct InstanceMethod {
  // expected-error @+1 {{'invalidDiffNoParams()' has no parameters to differentiate with respect to}}
  @differentiable
  func invalidDiffNoParams() -> Float {
    return 1
  }

  // expected-error @+1 {{cannot differentiate void function 'invalidDiffVoidResult(x:)'}}
  @differentiable
  func invalidDiffVoidResult(x: Float) {}
}

// Test instance methods for a `Differentiable` type.
struct DifferentiableInstanceMethod: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  @differentiable // ok
  func noParams() -> Float {
    return 1
  }
}

// Test subscript methods.
struct SubscriptMethod {
  @differentiable // ok
  subscript(implicitGetter x: Float) -> Float {
    return x
  }

  @differentiable // ok
  subscript(implicitGetterSetter x: Float) -> Float {
    get { return x }
    set {}
  }

  subscript(explicit x: Float) -> Float {
    @differentiable // ok
    get { return x }
    @differentiable // expected-error {{'@differentiable' attribute cannot be applied to this declaration}}
    set {}
  }

  subscript(x: Float, y: Float) -> Float {
    @differentiable // ok
    get { return x + y }
    @differentiable // expected-error {{'@differentiable' attribute cannot be applied to this declaration}}
    set {}
  }
}

// JVP

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(jvp: jvpSimpleJVP)
func jvpSimple(x: Float) -> Float {
  return x
}

func jvpSimpleJVP(x: Float) -> (Float, ((Float) -> Float)) {
  return (x, { v in v })
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: y, jvp: jvpWrtSubsetJVP)
func jvpWrtSubset1(x: Float, y: Float) -> Float {
  return x + y
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: (y), jvp: jvpWrtSubsetJVP)
func jvpWrtSubset2(x: Float, y: Float) -> Float {
  return x + y
}

func jvpWrtSubsetJVP(x: Float, y: Float) -> (Float, (Float) -> Float) {
  return (x + y, { v in v })
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(jvp: jvp2ParamsJVP)
func jvp2Params(x: Float, y: Float) -> Float {
  return x + y
}

func jvp2ParamsJVP(x: Float, y: Float) -> (Float, (Float, Float) -> Float) {
  return (x + y, { (a, b) in a + b })
}

// expected-error @+1 {{unknown parameter name 'y'}}
@differentiable(wrt: (y))
func jvpUnknownParam(x: Float) -> Float {
  return x
}

// expected-error @+1 {{parameters must be specified in original order}}
@differentiable(wrt: (y, x))
func jvpParamOrderNotIncreasing(x: Float, y: Float) -> Float {
  return x * y
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{'jvpWrongTypeJVP' does not have expected type '(Float) -> (Float, (Float.TangentVector) -> Float.TangentVector)' (aka '(Float) -> (Float, (Float) -> Float)'}}
@differentiable(jvp: jvpWrongTypeJVP)
func jvpWrongType(x: Float) -> Float {
  return x
}

func jvpWrongTypeJVP(x: Float) -> (Float, (Float) -> Int) {
  return (x, { v in Int(v) })
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{no differentiation parameters could be inferred; must differentiate with respect to at least one parameter conforming to 'Differentiable'}}
@differentiable(jvp: jvpSimpleJVP)
func jvpNonDiffParam(x: Int) -> Float {
  return Float(x)
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{can only differentiate functions with results that conform to 'Differentiable', but 'Int' does not conform to 'Differentiable'}}
@differentiable(jvp: jvpSimpleJVP)
func jvpNonDiffResult(x: Float) -> Int {
  return Int(x)
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{can only differentiate functions with results that conform to 'Differentiable', but '(Float, Int)' does not conform to 'Differentiable'}}
@differentiable(jvp: jvpSimpleJVP)
func jvpNonDiffResult2(x: Float) -> (Float, Int) {
  return (x, Int(x))
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{ambiguous reference to 'jvpAmbiguousVJP' in '@differentiable' attribute}}
@differentiable(jvp: jvpAmbiguousVJP)
func jvpAmbiguous(x: Float) -> Float {
  return x
}
func jvpAmbiguousVJP(_ x: Float) -> (Float, (Float) -> Float) {
  return (x, { $0 })
}
func jvpAmbiguousVJP(x: Float) -> (Float, (Float) -> Float) {
  return (x, { $0 })
}

class DifferentiableClassMethod {
  // Direct differentiation case.
  @differentiable
  func foo(_ x: Float) -> Float {
    return x
  }
}

struct JVPStruct {
  @differentiable
  let p: Float

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'funcJVP' does not have expected type '(JVPStruct) -> () -> (Double, (JVPStruct.TangentVector) -> Double.TangentVector)' (aka '(JVPStruct) -> () -> (Double, (JVPStruct) -> Double)'}}
  @differentiable(wrt: (self), jvp: funcJVP)
  func funcWrongType() -> Double {
    fatalError("unimplemented")
  }
}

extension JVPStruct {
  func funcJVP() -> (Float, (JVPStruct) -> Float) {
    fatalError("unimplemented")
  }
}

extension JVPStruct: AdditiveArithmetic {
  static var zero: JVPStruct { fatalError("unimplemented") }
  static func + (lhs: JVPStruct, rhs: JVPStruct) -> JVPStruct {
    fatalError("unimplemented")
  }
  static func - (lhs: JVPStruct, rhs: JVPStruct) -> JVPStruct {
    fatalError("unimplemented")
  }
  typealias Scalar = Float
  static func * (lhs: Float, rhs: JVPStruct) -> JVPStruct {
    fatalError("unimplemented")
  }
}

extension JVPStruct: Differentiable {
  typealias TangentVector = JVPStruct
}

extension JVPStruct {
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(wrt: x, jvp: wrtAllNonSelfJVP)
  func wrtAllNonSelf(x: Float) -> Float {
    return x + p
  }

  func wrtAllNonSelfJVP(x: Float) -> (Float, (Float) -> Float) {
    return (x + p, { v in v })
  }
}

extension JVPStruct {
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(wrt: (self, x), jvp: wrtAllJVP)
  func wrtAll(x: Float) -> Float {
    return x + p
  }

  func wrtAllJVP(x: Float) -> (Float, (JVPStruct, Float) -> Float) {
    return (x + p, { (a, b) in a.p + b })
  }
}

extension JVPStruct {
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(jvp: computedPropJVP)
  var computedPropOk1: Float {
    return 0
  }

  var computedPropOk2: Float {
    // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
    @differentiable(jvp: computedPropJVP)
    get {
      return 0
    }
  }

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'computedPropJVP' does not have expected type '(JVPStruct) -> () -> (Double, (JVPStruct.TangentVector) -> Double.TangentVector)' (aka '(JVPStruct) -> () -> (Double, (JVPStruct) -> Double)'}}
  @differentiable(jvp: computedPropJVP)
  var computedPropWrongType: Double {
    return 0
  }

  var computedPropWrongAccessor: Float {
    get {
      return 0
    }
    // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
    // expected-error @+1 {{'@differentiable' attribute cannot be applied to this declaration}}
    @differentiable(jvp: computedPropJVP)
    set {
      fatalError("unimplemented")
    }
  }

  func computedPropJVP() -> (Float, (JVPStruct) -> Float) {
    fatalError("unimplemented")
  }
}

// VJP

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: vjpSimpleVJP)
func vjpSimple(x: Float) -> Float {
  return x
}

func vjpSimpleVJP(x: Float) -> (Float, ((Float) -> Float)) {
  return (x, { v in v })
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: (y), vjp: vjpWrtSubsetVJP)
func vjpWrtSubset(x: Float, y: Float) -> Float {
  return x + y
}

func vjpWrtSubsetVJP(x: Float, y: Float) -> (Float, (Float) -> Float) {
  return (x + y, { v in v })
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: vjp2ParamsVJP)
func vjp2Params(x: Float, y: Float) -> Float {
  return x + y
}

func vjp2ParamsVJP(x: Float, y: Float) -> (Float, (Float) -> (Float, Float)) {
  return (x + y, { v in (v, v) })
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{'vjpWrongTypeVJP' does not have expected type '(Float) -> (Float, (Float.TangentVector) -> Float.TangentVector)' (aka '(Float) -> (Float, (Float) -> Float)'}}
@differentiable(vjp: vjpWrongTypeVJP)
func vjpWrongType(x: Float) -> Float {
  return x
}

func vjpWrongTypeVJP(x: Float) -> (Float, (Float) -> Int) {
  return (x, { v in Int(v) })
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{no differentiation parameters could be inferred; must differentiate with respect to at least one parameter conforming to 'Differentiable'}}
@differentiable(vjp: vjpSimpleVJP)
func vjpNonDiffParam(x: Int) -> Float {
  return Float(x)
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{can only differentiate functions with results that conform to 'Differentiable', but 'Int' does not conform to 'Differentiable'}}
@differentiable(vjp: vjpSimpleVJP)
func vjpNonDiffResult(x: Float) -> Int {
  return Int(x)
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{can only differentiate functions with results that conform to 'Differentiable', but '(Float, Int)' does not conform to 'Differentiable'}}
@differentiable(vjp: vjpSimpleVJP)
func vjpNonDiffResult2(x: Float) -> (Float, Int) {
  return (x, Int(x))
}

struct VJPStruct {
  let p: Float

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'funcVJP' does not have expected type '(VJPStruct) -> () -> (Double, (Double.TangentVector) -> VJPStruct.TangentVector)' (aka '(VJPStruct) -> () -> (Double, (Double) -> VJPStruct)'}}
  @differentiable(vjp: funcVJP)
  func funcWrongType() -> Double {
    fatalError("unimplemented")
  }
}

extension VJPStruct {
  func funcVJP() -> (Float, (Float) -> VJPStruct) {
    fatalError("unimplemented")
  }
}

extension VJPStruct: AdditiveArithmetic {
  static var zero: VJPStruct { fatalError("unimplemented") }
  static func + (lhs: VJPStruct, rhs: VJPStruct) -> VJPStruct {
    fatalError("unimplemented")
  }
  static func - (lhs: VJPStruct, rhs: VJPStruct) -> VJPStruct {
    fatalError("unimplemented")
  }
  typealias Scalar = Float
  static func * (lhs: Float, rhs: VJPStruct) -> VJPStruct {
    fatalError("unimplemented")
  }
}

extension VJPStruct: Differentiable {
  typealias TangentVector = VJPStruct
}

extension VJPStruct {
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(wrt: x, vjp: wrtAllNonSelfVJP)
  func wrtAllNonSelf(x: Float) -> Float {
    return x + p
  }

  func wrtAllNonSelfVJP(x: Float) -> (Float, (Float) -> Float) {
    return (x + p, { v in v })
  }
}

extension VJPStruct {
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(wrt: (self, x), vjp: wrtAllVJP)
  func wrtAll(x: Float) -> Float {
    return x + p
  }

  func wrtAllVJP(x: Float) -> (Float, (Float) -> (VJPStruct, Float)) {
    fatalError("unimplemented")
  }
}

extension VJPStruct {
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(vjp: computedPropVJP)
  var computedPropOk1: Float {
    return 0
  }

  var computedPropOk2: Float {
    // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
    @differentiable(vjp: computedPropVJP)
    get {
      return 0
    }
  }

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'computedPropVJP' does not have expected type '(VJPStruct) -> () -> (Double, (Double.TangentVector) -> VJPStruct.TangentVector)' (aka '(VJPStruct) -> () -> (Double, (Double) -> VJPStruct)'}}
  @differentiable(vjp: computedPropVJP)
  var computedPropWrongType: Double {
    return 0
  }

  var computedPropWrongAccessor: Float {
    get {
      return 0
    }
    // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
    // expected-error @+1 {{'@differentiable' attribute cannot be applied to this declaration}}
    @differentiable(vjp: computedPropVJP)
    set {
      fatalError("unimplemented")
    }
  }

  func computedPropVJP() -> (Float, (Float) -> VJPStruct) {
    fatalError("unimplemented")
  }
}

// expected-error @+2 {{empty 'where' clause in '@differentiable' attribute}}
// expected-error @+1 {{expected type}}
@differentiable(where)
func emptyWhereClause<T>(x: T) -> T {
  return x
}

// expected-error @+1 {{'where' clause is valid only when original function is generic 'nongenericWhereClause(x:)'}}
@differentiable(where T: Differentiable)
func nongenericWhereClause(x: Float) -> Float {
  return x
}

// expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(jvp: jvpWhere1, vjp: vjpWhere1 where T: Differentiable)
func where1<T>(x: T) -> T {
  return x
}
func jvpWhere1<T: Differentiable>(x: T) -> (T, (T.TangentVector) -> T.TangentVector) {
  return (x, { v in v })
}
func vjpWhere1<T: Differentiable>(x: T) -> (T, (T.TangentVector) -> T.TangentVector) {
  return (x, { v in v })
}

// Test derivative functions with result tuple type labels.
// expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(jvp: jvpResultLabels, vjp: vjpResultLabels)
func derivativeResultLabels(_ x: Float) -> Float {
  return x
}
func jvpResultLabels(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
  return (x, { $0 })
}
func vjpResultLabels(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}
struct ResultLabelTest {
  // expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(jvp: jvpResultLabels, vjp: vjpResultLabels)
  static func derivativeResultLabels(_ x: Float) -> Float {
    return x
  }
  static func jvpResultLabels(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
    return (x, { $0 })
  }
  static func vjpResultLabels(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (x, { $0 })
  }

  // expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(jvp: jvpResultLabels, vjp: vjpResultLabels)
  func derivativeResultLabels(_ x: Float) -> Float {
    return x
  }
  func jvpResultLabels(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
    return (x, { $0 })
  }
  func vjpResultLabels(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (x, { $0 })
  }
}

struct Tensor<Scalar>: AdditiveArithmetic {
  static var zero: Self { Self() }
  static func + (_: Self, _: Self) -> Self { Self() }
  static func - (_: Self, _: Self) -> Self { Self() }
}
extension Tensor: Differentiable where Scalar: Differentiable {
  typealias TangentVector = Self
}
// expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(jvp: jvpWhere2, vjp: vjpWhere2 where Scalar: Differentiable)
func where2<Scalar: Numeric>(x: Tensor<Scalar>) -> Tensor<Scalar> {
  return x
}
func jvpWhere2<Scalar: Numeric & Differentiable>(x: Tensor<Scalar>) -> (Tensor<Scalar>, (Tensor<Scalar>) -> Tensor<Scalar>) {
  return (x, { v in v })
}
func vjpWhere2<Scalar: Numeric & Differentiable>(x: Tensor<Scalar>) -> (Tensor<Scalar>, (Tensor<Scalar>) -> Tensor<Scalar>) {
  return (x, { v in v })
}

struct A<T> {
  struct B<U, V> {
    @differentiable(wrt: x where T: Differentiable, V: Differentiable, V.TangentVector == V)
    func whereInGenericContext<T>(x: T) -> T {
      return x
    }
  }
}

extension FloatingPoint {
  @differentiable(wrt: (self) where Self: Differentiable)
  func whereClauseExtension() -> Self {
    return self
  }
}
// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{'vjpNonvariadic' does not have expected type '(Float, Int32...) -> (Float, (Float.TangentVector) -> Float.TangentVector)' (aka '(Float, Int32...) -> (Float, (Float) -> Float)')}}
@differentiable(wrt: x, vjp: vjpNonvariadic)
func variadic(_ x: Float, indices: Int32...) -> Float {
  return x
}
func vjpNonvariadic(_ x: Float, indices: [Int32]) -> (Float, (Float) -> Float) {
  return (x, { $0 })
}

// expected-error @+3 {{type 'Scalar' constrained to non-protocol, non-class type 'Float'}}
// expected-error @+2 {{no differentiation parameters could be inferred; must differentiate with respect to at least one parameter conforming to 'Differentiable'}}
// expected-note @+1 {{use 'Scalar == Float' to require 'Scalar' to be 'Float'}}
@differentiable(where Scalar: Float)
func invalidRequirementConformance<Scalar>(x: Scalar) -> Scalar {
  return x
}

@differentiable(where T: AnyObject)
func invalidAnyObjectRequirement<T: Differentiable>(x: T) -> T {
  return x
}

// expected-error @+1 {{'@differentiable' attribute does not yet support layout requirements}}
@differentiable(where Scalar: _Trivial)
func invalidRequirementLayout<Scalar>(x: Scalar) -> Scalar {
  return x
}

// expected-error @+1 {{no differentiation parameters could be inferred; must differentiate with respect to at least one parameter conforming to 'Differentiable'}}
@differentiable
func missingConformance<T>(_ x: T) -> T {
  return x
}

protocol ProtocolRequirements: Differentiable {
  // expected-note @+2 {{protocol requires initializer 'init(x:y:)' with type '(x: Float, y: Float)'}}
  @differentiable
  init(x: Float, y: Float)

  // expected-note @+2 {{protocol requires initializer 'init(x:y:)' with type '(x: Float, y: Int)'}}
  @differentiable(wrt: x)
  init(x: Float, y: Int)

  // expected-note @+2 {{protocol requires function 'amb(x:y:)' with type '(Float, Float) -> Float';}}
  @differentiable
  func amb(x: Float, y: Float) -> Float

  // expected-note @+2 {{protocol requires function 'amb(x:y:)' with type '(Float, Int) -> Float';}}
  @differentiable(wrt: x)
  func amb(x: Float, y: Int) -> Float

  // expected-note @+3 {{protocol requires function 'f1'}}
  // expected-note @+2 {{overridden declaration is here}}
  @differentiable(wrt: (self, x))
  func f1(_ x: Float) -> Float

  // expected-note @+2 {{protocol requires function 'f2'}}
  @differentiable(wrt: (self, x, y))
  func f2(_ x: Float, _ y: Float) -> Float
}

protocol ProtocolRequirementsRefined: ProtocolRequirements {
  // expected-error @+1 {{overriding declaration is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  func f1(_ x: Float) -> Float
}

// expected-error @+1 {{does not conform to protocol 'ProtocolRequirements'}}
struct DiffAttrConformanceErrors: ProtocolRequirements {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  var x: Float
  var y: Float

  // FIXME(TF-284): Fix unexpected diagnostic.
  // expected-note @+2 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  // expected-note @+1 {{candidate has non-matching type '(x: Float, y: Float)'}}
  init(x: Float, y: Float) {
    self.x = x
    self.y = y
  }

  // FIXME(TF-284): Fix unexpected diagnostic.
  // expected-note @+2 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  // expected-note @+1 {{candidate has non-matching type '(x: Float, y: Int)'}}
  init(x: Float, y: Int) {
    self.x = x
    self.y = Float(y)
  }

  // expected-note @+2 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  // expected-note @+1 {{candidate has non-matching type '(Float, Float) -> Float'}}
  func amb(x: Float, y: Float) -> Float {
    return x
  }

  // expected-note @+2 {{candidate is missing attribute '@differentiable(wrt: x)'}} {{3-3=@differentiable(wrt: x) }}
  // expected-note @+1 {{candidate has non-matching type '(Float, Int) -> Float'}}
  func amb(x: Float, y: Int) -> Float {
    return x
  }

  // expected-note @+1 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  func f1(_ x: Float) -> Float {
    return x
  }

  // expected-note @+2 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  @differentiable(wrt: (self, x))
  func f2(_ x: Float, _ y: Float) -> Float {
    return x + y
  }
}

protocol ProtocolRequirementsWithDefault_NoConformingTypes {
  @differentiable
  func f1(_ x: Float) -> Float
}
extension ProtocolRequirementsWithDefault_NoConformingTypes {
  // TODO(TF-650): It would be nice to diagnose protocol default implementation
  // with missing `@differentiable` attribute.
  func f1(_ x: Float) -> Float { x }
}

protocol ProtocolRequirementsWithDefault {
  // expected-note @+2 {{protocol requires function 'f1'}}
  @differentiable
  func f1(_ x: Float) -> Float
}
extension ProtocolRequirementsWithDefault {
  // expected-note @+1 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  func f1(_ x: Float) -> Float { x }
}
// expected-error @+1 {{type 'DiffAttrConformanceErrors2' does not conform to protocol 'ProtocolRequirementsWithDefault'}}
struct DiffAttrConformanceErrors2: ProtocolRequirementsWithDefault {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  // expected-note @+1 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  func f1(_ x: Float) -> Float { x }
}

protocol NotRefiningDiffable {
  @differentiable(wrt: x)
  // expected-note @+1 {{protocol requires function 'a' with type '(Float) -> Float'; do you want to add a stub?}}
  func a(_ x: Float) -> Float
}

// expected-error @+1 {{type 'CertainlyNotDiffableWrtSelf' does not conform to protocol 'NotRefiningDiffable'}}
struct CertainlyNotDiffableWrtSelf: NotRefiningDiffable {
  // expected-note @+1 {{candidate is missing attribute '@differentiable'}} {{3-3=@differentiable }}
  func a(_ x: Float) -> Float { return x * 5.0 }
}


protocol TF285: Differentiable {
  @differentiable(wrt: (x, y))
  @differentiable(wrt: x)
  // expected-note @+1 {{protocol requires function 'foo(x:y:)' with type '(Float, Float) -> Float'; do you want to add a stub?}}
  func foo(x: Float, y: Float) -> Float
}

// expected-error @+1 {{type 'TF285MissingOneDiffAttr' does not conform to protocol 'TF285'}}
struct TF285MissingOneDiffAttr: TF285 {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  // Requirement is missing an attribute.
  @differentiable(wrt: x)
  // expected-note @+1 {{candidate is missing attribute '@differentiable(wrt: (x, y))}} {{3-3=@differentiable(wrt: (x, y)) }}
  func foo(x: Float, y: Float) -> Float {
    return x
  }
}

// TF-521: Test invalid `@differentiable` attribute due to invalid
// `Differentiable` conformance (`TangentVector` does not conform to
// `AdditiveArithmetic`).
struct TF_521<T: FloatingPoint> {
  var real: T
  var imaginary: T

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{can only differentiate functions with results that conform to 'Differentiable', but 'TF_521<T>' does not conform to 'Differentiable'}}
  @differentiable(vjp: _vjpInit where T: Differentiable, T == T.TangentVector)
  init(real: T = 0, imaginary: T = 0) {
    self.real = real
    self.imaginary = imaginary
  }
}
// expected-error @+1 {{type 'TF_521<T>' does not conform to protocol 'Differentiable'}}
extension TF_521: Differentiable where T: Differentiable {
  // expected-note @+1 {{possibly intended match 'TF_521<T>.TangentVector' does not conform to 'AdditiveArithmetic'}}
  typealias TangentVector = TF_521
  typealias AllDifferentiableVariables = TF_521
}
extension TF_521 where T: Differentiable, T == T.TangentVector {
  static func _vjpInit(real: T, imaginary: T) -> (TF_521, (TF_521) -> (T, T)) {
    return (TF_521(real: real, imaginary: imaginary), { ($0.real, $0.imaginary) })
  }
}
let _: @differentiable (Float, Float) -> TF_521<Float> = { r, i in
  TF_521(real: r, imaginary: i)
}

// TF-296: Infer `@differentiable` wrt parameters to be to all parameters that conform to `Differentiable`.

@differentiable
func infer1(_ a: Float, _ b: Int) -> Float {
  return a + Float(b)
}

@differentiable
func infer2(_ fn: @differentiable(Float) -> Float, x: Float) -> Float {
  return fn(x)
}

struct DiffableStruct: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  var a: Float

  @differentiable
  func fn(_ b: Float, _ c: Int) -> Float {
    return a + b + Float(c)
  }
}

struct NonDiffableStruct {
  var a: Float

  @differentiable
  func fn(_ b: Float) -> Float {
    return a + b
  }
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(linear, wrt: x, vjp: const3) // expected-error {{cannot specify 'vjp:' or 'jvp:' for linear functions; use '@transpose' attribute for transpose registration instead}}
func slope1(_ x: Float) -> Float {
  return 3 * x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(linear, wrt: x, jvp: const3) // expected-error {{cannot specify 'vjp:' or 'jvp:' for linear functions; use '@transpose' attribute for transpose registration instead}}
func slope2(_ x: Float) -> Float {
  return 3 * x
}

// expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(linear, jvp: const3, vjp: const3) // expected-error {{cannot specify 'vjp:' or 'jvp:' for linear functions; use '@transpose' attribute for transpose registration instead}}
func slope3(_ x: Float) -> Float {
  return 3 * x
}

// Check that `@differentiable` attribute rejects stored properties.
struct StoredProperty: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'@differentiable' attribute on stored property cannot specify 'jvp:' or 'vjp:'}}
  @differentiable(vjp: vjpStored)
  var stored: Float

  func vjpStored() -> (Float, (Float) -> TangentVector) {
    (stored, { _ in .zero })
  }
}

// Check that `@differentiable` attribute rejects non-`func` derivatives.
struct Struct: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{registered derivative 'computedPropertyVJP' must be a 'func' declaration}}
  @differentiable(vjp: computedPropertyVJP)
  func testComputedProperty() -> Float { 1 }
  var computedPropertyVJP: (Float, (Float) -> TangentVector) {
    (1, { _ in .zero })
  }

  // expected-error @+1 {{expected a vjp function name}}
  @differentiable(vjp: init)
  func testInitializer() -> Struct { self }
  init(_ x: Struct) {}

  // expected-error @+1 {{expected a vjp function name}}
  @differentiable(vjp: subscript)
  func testSubscript() -> Float { 1 }
  subscript() -> (Float, (Float) -> TangentVector) {
    (1, { _ in .zero })
  }
}

// Index based 'wrt:'

struct NumberWrtStruct: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  var a, b: Float

  @differentiable(wrt: 0) // ok
  @differentiable(wrt: 1) // ok
  func foo1(_ x: Float, _ y: Float) -> Float {
    return a*x + b*y
  }

  @differentiable(wrt: -1) // expected-error {{expected a parameter, which can be a function parameter name, parameter index, or 'self'}}
  @differentiable(wrt: (1, x)) // expected-error {{parameters must be specified in original order}}
  func foo2(_ x: Float, _ y: Float) -> Float {
    return a*x + b*y
  }

  @differentiable(wrt: (x, 1)) // ok
  @differentiable(wrt: (0)) // ok
  static func staticFoo1(_ x: Float, _ y: Float) -> Float {
    return x + y
  }

  @differentiable(wrt: (1, 1)) // expected-error {{parameters must be specified in original order}}
  @differentiable(wrt: (2)) // expected-error {{parameter index is larger than total number of parameters}}
  static func staticFoo2(_ x: Float, _ y: Float) -> Float {
    return x + y
  }
}

@differentiable(wrt: y) // ok
func two1(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (x, y)) // ok
func two2(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (0, y)) // ok
func two3(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (x, 1)) // ok
func two4(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (0, 1)) // ok
func two5(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: 2) // expected-error {{parameter index is larger than total number of parameters}}
func two6(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (1, 0)) // expected-error {{parameters must be specified in original order}}
func two7(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (1, x)) // expected-error {{parameters must be specified in original order}}
func two8(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (y, 0)) // expected-error {{parameters must be specified in original order}}
func two9(x: Float, y: Float) -> Float {
  return x + y
}

// Test refining protocol requirements with `@differentiable` attribute.

public protocol Distribution {
  associatedtype Value
  func logProbability(of value: Value) -> Float
}

public protocol DifferentiableDistribution: Differentiable, Distribution {
  // expected-note @+2 {{overridden declaration is here}}
  @differentiable(wrt: self)
  func logProbability(of value: Value) -> Float
}

// Adding a more general `@differentiable` attribute.
public protocol DoubleDifferentiableDistribution: DifferentiableDistribution
  where Value: Differentiable {
  // expected-error @+1 {{overriding declaration is missing attribute '@differentiable(wrt: self)'}} {{3-3=@differentiable(wrt: self) }}
  func logProbability(of value: Value) -> Float
}

// Test protocol requirement `@differentiable` attribute unsupported features.

protocol ProtocolRequirementUnsupported: Differentiable {
  associatedtype Scalar

  // expected-error @+1 {{'@differentiable' attribute on protocol requirement cannot specify 'where' clause}}
  @differentiable(where Scalar: Differentiable)
  func unsupportedWhereClause(value: Scalar) -> Float

  // expected-warning @+2 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'@differentiable' attribute on protocol requirement cannot specify 'jvp:' or 'vjp:'}}
  @differentiable(wrt: x, jvp: dfoo, vjp: dfoo)
  func unsupportedDerivatives(_ x: Float) -> Float
}
extension ProtocolRequirementUnsupported {
  func dfoo(_ x: Float) -> (Float, (Float) -> Float) {
    (x, { $0 })
  }
}

// Classes.

class Super: Differentiable {
  typealias TangentVector = DummyTangentVector
  func move(along _: TangentVector) {}

  var base: Float

  // expected-error @+1 {{'@differentiable' attribute cannot be declared on 'init' in a non-final class; consider making 'Super' final}}
  @differentiable
  init(base: Float) {
    self.base = base
  }

  // NOTE(TF-1040): `@differentiable` attribute on class methods currently
  // does two orthogonal things:
  // - Requests derivative generation for the class method.
  // - Adds JVP/VJP vtable entries for the class method.
  // There's currently no way using `@differentiable` to do only one of the
  // above.
  @differentiable
  func testClassMethod(_ x: Float) -> Float { x }

  @differentiable
  final func testFinalMethod(_ x: Float) -> Float { x }

  @differentiable
  static func testStaticMethod(_ x: Float) -> Float { x }

  @differentiable(wrt: (self, x))
  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(wrt: x, vjp: vjp)
  // expected-note @+1 2 {{overridden declaration is here}}
  func testMissingAttributes(_ x: Float) -> Float { x }

  // expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  @differentiable(wrt: x, vjp: vjp)
  func testSuperclassDerivatives(_ x: Float) -> Float { x }

  final func vjp(_ x: Float) -> (Float, (Float) -> Float) {
    fatalError()
  }

  // Test duplicate attributes with different derivative generic signatures.
  // expected-error @+1 {{duplicate '@differentiable' attribute with same parameters}}
  @differentiable(wrt: x where T: Differentiable)
  // expected-note @+1 {{other attribute declared here}}
  @differentiable(wrt: x)
  func instanceMethod<T>(_ x: Float, y: T) -> Float { x }

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'@differentiable' attribute cannot be declared on class members returning 'Self'}}
  @differentiable(vjp: vjpDynamicSelfResult)
  func dynamicSelfResult() -> Self { self }

  // TODO(TF-632): Fix "'TangentVector' is not a member type of 'Self'" diagnostic.
  // The underlying error should appear instead:
  // "covariant 'Self' can only appear at the top level of method result type".
  // expected-error @+1 2 {{'TangentVector' is not a member type of 'Self'}}
  func vjpDynamicSelfResult() -> (Self, (Self.TangentVector) -> Self.TangentVector) {
    return (self, { $0 })
  }
}

class Sub: Super {
  // expected-error @+2 {{overriding declaration is missing attribute '@differentiable(wrt: x)'}} {{12-12=@differentiable(wrt: x) }}
  // expected-error @+1 {{overriding declaration is missing attribute '@differentiable'}} {{12-12=@differentiable }}
  override func testMissingAttributes(_ x: Float) -> Float { x }

  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{'vjp' is not defined in the current type context}}
  @differentiable(wrt: x, vjp: vjp)
  override func testSuperclassDerivatives(_ x: Float) -> Float { x }
}

final class FinalClass: Differentiable {
  typealias TangentVector = DummyTangentVector
  func move(along _: TangentVector) {}

  var base: Float

  @differentiable
  init(base: Float) {
    self.base = base
  }
}

// Test `inout` parameters.

@differentiable(wrt: y)
func inoutVoid(x: Float, y: inout Float) {}

// expected-error @+1 {{cannot yet differentiate functions with more than one semantic result (formal function result or 'inout' parameter)}}
@differentiable
func multipleSemanticResults(_ x: inout Float) -> Float { x }

// expected-error @+1 {{cannot yet differentiate functions with more than one semantic result (formal function result or 'inout' parameter)}}
@differentiable(wrt: y)
func swap(x: inout Float, y: inout Float) {}

struct InoutParameters: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}
}

extension InoutParameters {
  @differentiable
  static func staticMethod(_ lhs: inout Self, rhs: Self) {}

  // expected-error @+1 {{cannot yet differentiate functions with more than one semantic result (formal function result or 'inout' parameter)}}
  @differentiable
  static func multipleSemanticResults(_ lhs: inout Self, rhs: Self) -> Self {}
}

extension InoutParameters {
  @differentiable
  mutating func mutatingMethod(_ other: Self) {}

  // expected-error @+1 {{cannot yet differentiate functions with more than one semantic result (formal function result or 'inout' parameter)}}
  @differentiable
  mutating func mutatingMethod(_ other: Self) -> Self {}
}

// Test unsupported accessors: `set`, `_read`, `_modify`.

struct UnsupportedAccessors: Differentiable {
  typealias TangentVector = DummyTangentVector
  mutating func move(along _: TangentVector) {}

  var stored: Float
  var computed: Float {
    // `set` has an `inout` parameter: `(inout Self) -> (Float) -> ()`.
    // expected-error @+1 {{'@differentiable' attribute cannot be applied to this declaration}}
    @differentiable
    set { stored = newValue }

    // `_read` is a coroutine: `(Self) -> () -> ()`.
    // expected-error @+1 {{'@differentiable' attribute cannot be applied to this declaration}}
    @differentiable
    _read { yield stored }

    // `_modify` is a coroutine: `(inout Self) -> () -> ()`.
    // expected-error @+1 {{'@differentiable' attribute cannot be applied to this declaration}}
    @differentiable
    _modify { yield &stored }
  }
}
