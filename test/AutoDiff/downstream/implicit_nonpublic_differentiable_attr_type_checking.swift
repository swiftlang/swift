// RUN: %target-swift-frontend -print-ast -verify %s | %FileCheck %s

// Test implicit `@differentiable` attributes for non-public protocol witnesses.

protocol InternalProtocol: Differentiable {
  // expected-note @+3 {{protocol requires function 'publicMethod' with type '(Float) -> Float'}}
  @differentiable(wrt: self)
  @differentiable(wrt: (self, x))
  func publicMethod(_ x: Float) -> Float

  @differentiable(wrt: self)
  @differentiable(wrt: (self, x))
  func internalMethod(_ x: Float) -> Float
}

// expected-error @+1 {{type 'PublicConformingStruct' does not conform to protocol 'InternalProtocol'}}
public struct PublicConformingStruct: InternalProtocol {
  // Expected: error for missing `@differentiable` attribute on public protocol witness.
  // expected-note @+1 {{candidate is missing attribute '@differentiable'}}
  public func publicMethod(_ x: Float) -> Float {
    x
  }

  // Expected: no error for missing `@differentiable` attribute on internal protocol witness.
  // Implicit `@differentiable` attributes should be created.
  func internalMethod(_ x: Float) -> Float {
    x
  }
}

// CHECK-LABEL: public struct PublicConformingStruct : InternalProtocol {
// CHECK:   public func publicMethod(_ x: Float) -> Float
// CHECK:   @differentiable(wrt: (self, x))
// CHECK:   @differentiable(wrt: self)
// CHECK:   internal func internalMethod(_ x: Float) -> Float
// CHECK: }
