// RUN: %target-swift-emit-sil -Xllvm -debug-only=differentiation 2>&1 %s | %FileCheck %s

// Check that `@noDerivative` struct projections have "NONE" activity.

struct HasNoDerivativeProperty: Differentiable {
  var x: Float
  @noDerivative var y: Float
}
@differentiable
func testNoDerivativeStructProjection(_ s: HasNoDerivativeProperty) -> Float {
  var tmp = s
  tmp.y = tmp.x
  return tmp.x
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}testNoDerivativeStructProjection{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $HasNoDerivativeProperty
// CHECK: [ACTIVE]   %2 = alloc_stack $HasNoDerivativeProperty, var, name "tmp"
// CHECK: [ACTIVE]   %4 = begin_access [read] [static] %2 : $*HasNoDerivativeProperty
// CHECK: [ACTIVE]   %5 = struct_element_addr %4 : $*HasNoDerivativeProperty, #HasNoDerivativeProperty.x
// CHECK: [VARIED]   %6 = load [trivial] %5 : $*Float
// CHECK: [ACTIVE]   %8 = begin_access [modify] [static] %2 : $*HasNoDerivativeProperty
// CHECK: [NONE]   %9 = struct_element_addr %8 : $*HasNoDerivativeProperty, #HasNoDerivativeProperty.y
// CHECK: [ACTIVE]   %12 = begin_access [read] [static] %2 : $*HasNoDerivativeProperty
// CHECK: [ACTIVE]   %13 = struct_element_addr %12 : $*HasNoDerivativeProperty, #HasNoDerivativeProperty.x
// CHECK: [ACTIVE]   %14 = load [trivial] %13 : $*Float

// TF-781: check activity analysis for active local address + nested conditionals.

@differentiable(wrt: x)
func TF_781_function(_ x: Float, _ y: Float) -> Float {
  var result = y
  if true {
    if true {
      result = result * x
    }
  }
  return result
}

// CHECK-LABEL: [AD] Activity info for ${{.*}}TF_781_function{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [USEFUL] %1 = argument of bb0 : $Float
// CHECK: [ACTIVE]   %4 = alloc_stack $Float, var, name "result"
// CHECK: [ACTIVE]   %19 = begin_access [read] [static] %4 : $*Float
// CHECK: [ACTIVE]   %20 = load [trivial] %19 : $*Float
// CHECK: [ACTIVE]   %23 = apply %22(%20, %0, %18) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %24 = begin_access [modify] [static] %4 : $*Float
// CHECK: [ACTIVE]   %31 = begin_access [read] [static] %4 : $*Float
// CHECK: [ACTIVE]   %32 = load [trivial] %31 : $*Float
