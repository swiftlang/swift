import StdlibUnittest
import DifferentiationUnittest

import module1
import module2

var DerivativeRegistrationTests = TestSuite("DerivativeRegistration")

@_semantics("autodiff.opaque")
func unary(x: Tracked<Float>) -> Tracked<Float> {
  return x
}
@derivative(of: unary)
func _vjpUnary(x: Tracked<Float>) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
  return (value: x, pullback: { v in v })
}
DerivativeRegistrationTests.testWithLeakChecking("UnaryFreeFunction") {
  expectEqual(1, gradient(at: 3.0, in: unary))
}

@_semantics("autodiff.opaque")
func multiply(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
  return x * y
}
@derivative(of: multiply)
func _vjpMultiply(_ x: Tracked<Float>, _ y: Tracked<Float>)
  -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Tracked<Float>, Tracked<Float>)) {
  return (x * y, { v in (v * y, v * x) })
}
DerivativeRegistrationTests.testWithLeakChecking("BinaryFreeFunction") {
  expectEqual((3.0, 2.0), gradient(at: 2.0, 3.0, in: { x, y in multiply(x, y) }))
}

struct Wrapper : Differentiable {
  var float: Tracked<Float>
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  init(_ x: Tracked<Float>, _ y: Tracked<Float>) {
    self.float = x * y
  }

  @derivative(of: init(_:_:))
  static func _vjpInit(_ x: Tracked<Float>, _ y: Tracked<Float>)
    -> (value: Self, pullback: (TangentVector) -> (Tracked<Float>, Tracked<Float>)) {
    return (.init(x, y), { v in (v.float * y, v.float * x) })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("Initializer") {
  let v = Wrapper.TangentVector(float: 1)
  let (𝛁x, 𝛁y) = pullback(at: 3, 4, in: { x, y in Wrapper(x, y) })(v)
  expectEqual(4, 𝛁x)
  expectEqual(3, 𝛁y)
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  static func multiply(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    return x * y
  }

  @derivative(of: multiply)
  static func _vjpMultiply(_ x: Tracked<Float>, _ y: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Tracked<Float>, Tracked<Float>)) {
    return (x * y, { v in (v * y, v * x) })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("StaticMethod") {
  expectEqual((3.0, 2.0), gradient(at: 2.0, 3.0, in: { x, y in Wrapper.multiply(x, y) }))
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  func multiply(_ x: Tracked<Float>) -> Tracked<Float> {
    return float * x
  }

  @derivative(of: multiply)
  func _vjpMultiply(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Wrapper.TangentVector, Tracked<Float>)) {
    return (float * x, { v in
      (TangentVector(float: v * x), v * self.float)
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("InstanceMethod") {
  let x: Tracked<Float> = 2
  let wrapper = Wrapper(float: 3)
  let (𝛁wrapper, 𝛁x) = gradient(at: wrapper, x) { wrapper, x in wrapper.multiply(x) }
  expectEqual(Wrapper.TangentVector(float: 2), 𝛁wrapper)
  expectEqual(3, 𝛁x)
}

extension Wrapper {
  subscript(_ x: Tracked<Float>) -> Tracked<Float> {
    @_semantics("autodiff.opaque")
    get { float * x }
    set {}
  }

  @derivative(of: subscript(_:))
  func _vjpSubscript(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Wrapper.TangentVector, Tracked<Float>)) {
    return (self[x], { v in
      (TangentVector(float: v * x), v * self.float)
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("Subscript") {
  let x: Tracked<Float> = 2
  let wrapper = Wrapper(float: 3)
  let (𝛁wrapper, 𝛁x) = gradient(at: wrapper, x) { wrapper, x in wrapper[x] }
  expectEqual(Wrapper.TangentVector(float: 2), 𝛁wrapper)
  expectEqual(3, 𝛁x)
}

extension Wrapper {
  var computedProperty: Tracked<Float> {
    @_semantics("autodiff.opaque")
    get { float * float }
    set {}
  }

  @derivative(of: computedProperty)
  func _vjpComputedProperty()
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Wrapper.TangentVector) {
    return (computedProperty, { [f = self.float] v in
      TangentVector(float: v * (f + f))
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("ComputedProperty") {
  let wrapper = Wrapper(float: 3)
  let 𝛁wrapper = gradient(at: wrapper) { wrapper in wrapper.computedProperty }
  expectEqual(Wrapper.TangentVector(float: 6), 𝛁wrapper)
}

struct Generic<T> {
  @differentiable // derivative generic signature: none
  func instanceMethod(_ x: Tracked<Float>) -> Tracked<Float> {
    x
  }
}
extension Generic {
  @derivative(of: instanceMethod) // derivative generic signature: <T>
  func vjpInstanceMethod(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
    (x, { v in 1000 })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("DerivativeGenericSignature") {
  let generic = Generic<Float>()
  let x: Tracked<Float> = 3
  let dx = gradient(at: x) { x in generic.instanceMethod(x) }
  expectEqual(1000, dx)
}

// MARK: - Non-overlapping private registration.

enum FunctionWithPrivateDerivativesInSeprateExtensions {
  static let expectedGradientFromExtension1: Float = 10
  static let expectedGradientFromExtension2: Float = 20

  static func f(_ x: Float) -> Float { x }
}

// Extension1
extension FunctionWithPrivateDerivativesInSeprateExtensions {
  @derivative(of: f)
  static private func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradientFromExtension1 * $0 })
  }

  static func gradFFromExtension1(_ x: Float) -> Float {
    gradient(at: x, in: f)
  }
}

// TODO(TF-1069): "a derivative already exists for 'f'" should not happen.
// Extension2
// extension FunctionWithPrivateDerivativesInSeprateExtensions {
//   @derivative(of: f)
//   static private func df2(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
//     (x, { expectedGradientFromExtension2 * $0 })
//   }
//
//   static func gradFFromExtension2(_ x: Float) -> Float {
//     gradient(at: x, in: f)
//   }
// }

DerivativeRegistrationTests.testWithLeakChecking("FunctionWithPrivateDerivativesInSeprateExtensions") {
  expectEqual(
    FunctionWithPrivateDerivativesInSeprateExtensions.expectedGradientFromExtension1,
    FunctionWithPrivateDerivativesInSeprateExtensions.gradFFromExtension1(0))

  // TODO(TF-1069): Enable this.
  //expectEqual(
  //  FunctionWithPrivateDerivativesInSeprateExtensions.expectedGradientFromExtension2,
  //  FunctionWithPrivateDerivativesInSeprateExtensions.gradFFromExtension2(0))
}

// MARK: - Cross-file registration.

extension FunctionInOtherFile_FileprivateDerivatives {
  @derivative(of: f)
  static fileprivate func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradientFromMainFile * $0 })
  }
}

DerivativeRegistrationTests.testWithLeakChecking("FunctionInOtherFile_FileprivateDerivatives") {
  // TODO(TF-1068): Enable this.
  //expectEqual(
  //  FunctionInOtherFile_FileprivateDerivatives.expectedGradientFromOtherFile,
  //  FunctionInOtherFile_FileprivateDerivatives.gradFFromOtherFile(0))
  expectEqual(
    FunctionInOtherFile_FileprivateDerivatives.expectedGradientFromMainFile,
    gradient(at: 0, in: FunctionInOtherFile_FileprivateDerivatives.f))
}

extension FunctionInOtherFile_DerivativeInMainFile {
  @derivative(of: f)
  static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradient * $0 })
  }
}

DerivativeRegistrationTests.testWithLeakChecking("FunctionInOtherFile_DerivativeInMainFile") {
  expectEqual(
    FunctionInOtherFile_DerivativeInMainFile.expectedGradient,
    gradient(at: 0, in: FunctionInOtherFile_DerivativeInMainFile.f))
  expectEqual(
    FunctionInOtherFile_DerivativeInMainFile.expectedGradient,
    FunctionInOtherFile_DerivativeInMainFile.gradFFromOtherFile(0))
}

// MARK: - Cross-module registration.

extension FunctionInModule1_InternalDerivatives {
  // TODO(TF-1068): This causes duplicate symbol linker errors.
  // TODO(TF-1067): Why is @usableFromInline necessary?
  // @derivative(of: f)
  // @usableFromInline
  // static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  //   (x, { expectedGradientFromMain * $0 })
  // }
}

DerivativeRegistrationTests.testWithLeakChecking("FunctionInModule1_InternalDerivatives") {
  expectEqual(
    FunctionInModule1_InternalDerivatives.expectedGradientFromModule1,
    FunctionInModule1_InternalDerivatives.gradFFromModule1(0))

  // TODO(TF-1068): Enable these.
  // expectEqual(
  //   FunctionInModule2_InternalDerivatives.expectedGradientFromModule2,
  //   FunctionInModule2_InternalDerivatives.gradFFromModule2(0))
  //expectEqual(
  //  FunctionInModule1_InternalDerivatives.expectedGradientFromMain,
  //  gradient(at: 0, in: FunctionInModule1_InternalDerivatives.f))
}

DerivativeRegistrationTests.testWithLeakChecking("FunctionInModule1_PublicDerivativeInModule2") {
  expectEqual(
    FunctionInModule1_PublicDerivativeInModule2.expectedGradient,
    gradient(at: 0, in: FunctionInModule1_PublicDerivativeInModule2.f))
}

public extension FunctionInModule1_PublicDerivativeInOtherFile {
  static func gradFFromMain(_ x: Float) -> Float { gradient(at: x, in: f) }
}

DerivativeRegistrationTests.testWithLeakChecking("FunctionInModule1_PublicDerivativeInOtherFile") {
  expectEqual(
    FunctionInModule1_PublicDerivativeInOtherFile.expectedGradient,
    FunctionInModule1_PublicDerivativeInOtherFile.gradFFromModule1(0))
  expectEqual(
    FunctionInModule1_PublicDerivativeInOtherFile.expectedGradient,
    FunctionInModule1_PublicDerivativeInOtherFile.gradFFromModule1OtherFile(0))
  expectEqual(
    FunctionInModule1_PublicDerivativeInOtherFile.expectedGradient,
    FunctionInModule1_PublicDerivativeInOtherFile.gradFFromModule2(0))
  expectEqual(
    FunctionInModule1_PublicDerivativeInOtherFile.expectedGradient,
    FunctionInModule1_PublicDerivativeInOtherFile.gradFFromMain(0))
}

public extension FunctionInModule1_PublicDerivativeImplementedInternally {
  static func gradFFromMain(_ x: Float) -> Float { gradient(at: x, in: f) }
}

DerivativeRegistrationTests.testWithLeakChecking("FunctionInModule1_PublicDerivativeImplementedInternally") {
  expectEqual(
    FunctionInModule1_PublicDerivativeImplementedInternally.expectedGradient,
    FunctionInModule1_PublicDerivativeImplementedInternally.gradFFromModule1(0))
  expectEqual(
    FunctionInModule1_PublicDerivativeImplementedInternally.expectedGradient,
    FunctionInModule1_PublicDerivativeImplementedInternally.gradFFromMain(0))
}

runAllTests()
