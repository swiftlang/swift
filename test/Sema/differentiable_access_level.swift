// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s

// TF-1077: Verify access levels of `TangentVector` types and their memberwise
// initializers, synthesized during `Differentiable` derived conformances.

// `TangentVector` memberwise initializer access level should default to true,
// for usability.

public struct PublicStruct: Differentiable {}
internal struct InternalStruct: Differentiable {}
private struct PrivateStruct: Differentiable {}

// CHECK-LABEL: public struct PublicStruct : Differentiable {
// CHECK:   internal init()
// CHECK:   public struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
// CHECK:     public init()
// CHECK:   }
// CHECK: }

// CHECK-LABEL: internal struct InternalStruct : Differentiable {
// CHECK:   internal init()
// CHECK:   internal struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
// CHECK:     public init()
// CHECK:   }
// CHECK: }

// CHECK-LABEL: private struct PrivateStruct : Differentiable {
// CHECK:   internal init()
// CHECK:   fileprivate struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
// CHECK:     public init()
// CHECK:   }
// CHECK: }

public class PublicClass: Differentiable {}
internal class InternalClass: Differentiable {}
private class PrivateClass: Differentiable {}

// CHECK-LABEL: public class PublicClass : Differentiable {
// CHECK:   internal init()
// CHECK:   public struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
// CHECK:     public init()
// CHECK:   }
// CHECK: }

// CHECK-LABEL: internal class InternalClass : Differentiable {
// CHECK:   internal init()
// CHECK:   internal struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
// CHECK:     public init()
// CHECK:   }
// CHECK: }

// CHECK-LABEL: private class PrivateClass : Differentiable {
// CHECK:   internal init()
// CHECK:   fileprivate struct TangentVector : Differentiable, AdditiveArithmetic, PointwiseMultiplicative, ElementaryFunctions {
// CHECK:     public init()
// CHECK:   }
// CHECK: }
