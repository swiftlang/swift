// RUN: %target-swift-frontend -print-ast -verify %s | %FileCheck %s

// Test implicit `@differentiable` attributes for non-public protocol witnesses.

protocol InternalProtocol: Differentiable {
  @differentiable(wrt: self)
  @differentiable(wrt: (self, x))
  func internalMethod(_ x: Float) -> Float
}

public struct PublicConformingStruct: InternalProtocol {
  // Expected: no error for missing `@differentiable` attribute on internal protocol witness.
  // Implicit `@differentiable` attributes should be created.
  func internalMethod(_ x: Float) -> Float {
    x
  }
}

// CHECK-LABEL: public struct PublicConformingStruct : InternalProtocol {
// CHECK:   @differentiable(wrt: (self, x))
// CHECK:   @differentiable(wrt: self)
// CHECK:   internal func internalMethod(_ x: Float) -> Float
// CHECK: }
