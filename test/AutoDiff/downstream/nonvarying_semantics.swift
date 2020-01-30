// RUN: %target-swift-emit-sil -verify -Xllvm -debug-only=differentiation 2>&1 %s | %FileCheck %s
// REQUIRES: asserts

// Test approaches for affecting activity analysis (non-varying semantics):
// - `@noDerivative` on declaration
// - `@_semantics("autodiff.nonvarying")` on declaration
// - `withoutDerivative(at:)` at use site

extension Float {
  // No non-varying semantics.
  var int: Int { Int(self) }

  // Non-varying semantics.
  @noDerivative
  var intNoDerivative: Int { int }

  // Non-varying semantics.
  @_semantics("autodiff.nonvarying")
  var intNonvarying: Int { int }
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
@_silgen_name("id")
// expected-note @+1 {{when differentiating this function definition}}
func id(_ x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
  return Float(x.int)
}

// CHECK-LABEL: [AD] Activity info for id at (source=0 parameters=(0))
// CHECK: bb0:
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [USEFUL]   %2 = metatype $@thin Float.Type
// CHECK: [NONE]   // function_ref Float.int.getter
// CHECK: [ACTIVE]   %4 = apply %3(%0) : $@convention(method) (Float) -> Int
// CHECK: [NONE]   // function_ref Float.init(_:)
// CHECK: [ACTIVE]   %6 = apply %5(%4, %2) : $@convention(method) (Int, @thin Float.Type) -> Float

@differentiable
@_silgen_name("idWithoutDerivativeAt")
func idWithoutDerivativeAt(_ x: Float) -> Float {
  return Float(withoutDerivative(at: x.int))
}

// CHECK-LABEL: [AD] Activity info for idWithoutDerivativeAt at (source=0 parameters=(0))
// CHECK: bb0:
// CHECK: [VARIED] %0 = argument of bb0 : $Float
// CHECK: [USEFUL]   %2 = metatype $@thin Float.Type
// CHECK: [USEFUL]   %3 = alloc_stack $Int
// CHECK: [NONE]   // function_ref Float.int.getter
// CHECK: [VARIED]   %5 = apply %4(%0) : $@convention(method) (Float) -> Int
// CHECK: [VARIED]   %6 = alloc_stack $Int
// CHECK: [NONE]   // function_ref withoutDerivative<A>(at:)
// CHECK: [NONE]   %9 = apply %8<Int>(%3, %6) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: [USEFUL]   %11 = load [trivial] %3 : $*Int
// CHECK: [NONE]   // function_ref Float.init(_:)
// CHECK: [USEFUL]   %13 = apply %12(%11, %2) : $@convention(method) (Int, @thin Float.Type) -> Float

@differentiable
@_silgen_name("idNoDerivative")
func idNoDerivative(_ x: Float) -> Float {
  return Float(x.intNoDerivative)
}

// CHECK-LABEL: [AD] Activity info for idNoDerivative at (source=0 parameters=(0))
// CHECK: bb0:
// CHECK: [VARIED] %0 = argument of bb0 : $Float
// CHECK: [USEFUL]   %2 = metatype $@thin Float.Type
// CHECK: [NONE]   // function_ref Float.intNoDerivative.getter
// CHECK: [USEFUL]   %4 = apply %3(%0) : $@convention(method) (Float) -> Int
// CHECK: [NONE]   // function_ref Float.init(_:)
// CHECK: [USEFUL]   %6 = apply %5(%4, %2) : $@convention(method) (Int, @thin Float.Type) -> Float

@differentiable
@_silgen_name("idNonvaryingSemantics")
func idNonvaryingSemantics(_ x: Float) -> Float {
  return Float(x.intNonvarying)
}

// CHECK-LABEL: [AD] Activity info for idNonvaryingSemantics at (source=0 parameters=(0))
// CHECK: bb0:
// CHECK: [VARIED] %0 = argument of bb0 : $Float
// CHECK: [USEFUL]   %2 = metatype $@thin Float.Type
// CHECK: [NONE]   // function_ref Float.intNonvarying.getter
// CHECK: [USEFUL]   %4 = apply %3(%0) : $@convention(method) (Float) -> Int
// CHECK: [NONE]   // function_ref Float.init(_:)
// CHECK: [USEFUL]   %6 = apply %5(%4, %2) : $@convention(method) (Int, @thin Float.Type) -> Float
