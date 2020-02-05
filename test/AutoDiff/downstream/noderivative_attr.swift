// RUN: %target-swift-frontend -emit-silgen -verify %s | %FileCheck %s
// REQUIRES: asserts

@noDerivative var flag: Bool

struct NotDifferentiable {
  @noDerivative var stored: Float

  @noDerivative
  var computedProperty: Float {
    get { 1 }
    set {}
    _modify { yield &stored }
  }

  @noDerivative
  func instanceMethod(_ x: Float) -> Float { x }

  @noDerivative
  static func staticMethod(_ x: Float) -> Float { x }

  @noDerivative
  subscript(_ x: Float) -> Float {
    get { 1 }
    set {}
    _modify { yield &stored }
  }
}

// CHECK-LABEL: struct NotDifferentiable {
// CHECK:   @noDerivative @_hasStorage var stored: Float { get set }
// CHECK:   @noDerivative var computedProperty: Float { get set _modify }
// CHECK:   @noDerivative func instanceMethod(_ x: Float) -> Float
// CHECK:   @noDerivative static func staticMethod(_ x: Float) -> Float
// CHECK:   @noDerivative subscript(x: Float) -> Float { get set _modify }
// CHECK: }

// CHECK-LABEL: // NotDifferentiable.computedProperty.getter
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableV16computedPropertySfvg : $@convention(method) (NotDifferentiable) -> Float

// CHECK-LABEL: // NotDifferentiable.computedProperty.setter
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableV16computedPropertySfvs : $@convention(method) (Float, @inout NotDifferentiable) -> ()

// CHECK-LABEL: // NotDifferentiable.computedProperty.modify
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableV16computedPropertySfvM : $@yield_once @convention(method) (@inout NotDifferentiable) -> @yields @inout Float

// CHECK-LABEL: // NotDifferentiable.instanceMethod(_:)
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableV14instanceMethodyS2fF : $@convention(method) (Float, NotDifferentiable) -> Float

// CHECK-LABEL: // static NotDifferentiable.staticMethod(_:)
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableV12staticMethodyS2fFZ : $@convention(method) (Float, @thin NotDifferentiable.Type) -> Float

// CHECK-LABEL: // NotDifferentiable.subscript.getter
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableVyS2fcig : $@convention(method) (Float, NotDifferentiable) -> Float

// CHECK-LABEL: // NotDifferentiable.subscript.setter
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableVyS2fcis : $@convention(method) (Float, Float, @inout NotDifferentiable) -> ()

// CHECK-LABEL: // NotDifferentiable.subscript.modify
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @$s17noderivative_attr17NotDifferentiableVyS2fciM : $@yield_once @convention(method) (Float, @inout NotDifferentiable) -> @yields @inout Float

struct Bar: Differentiable {
  @noDerivative var stored: Float
}

// Test TF-152: derived conformances "no interface type set" crasher.
struct TF_152: Differentiable {
  @differentiable(wrt: bar)
  func applied(to input: Float, bar: TF_152_Bar) -> Float {
    return input
  }
}
struct TF_152_Bar: Differentiable {
  @noDerivative let dense: Float
}
