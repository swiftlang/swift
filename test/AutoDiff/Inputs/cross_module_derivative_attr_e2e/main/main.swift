import StdlibUnittest
import DifferentiationUnittest

import module1
import module2

var DerivativeRegistrationTests = TestSuite("DerivativeRegistration")

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
