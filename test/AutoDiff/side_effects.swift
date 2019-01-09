// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -debug-only=differentiation %s 2>&1 || exit 0 | %FileCheck %s

func simpleStoreLoad(x: Float) -> Float {
  var y = x
  y = x + 1
  // expected-error @+1 {{expression is not differentiable}}
  return y
}
let _: @autodiff (Float) -> Float = simpleStoreLoad(x:)

// CHECK-LABEL: [AD] Activity info for ${{.*}}simpleStoreLoad{{.*}} at (source=0 parameters=(0))
// CHECK: [ACTIVE] %0 = argument of bb0 : $Float
// CHECK: [ACTIVE]   %2 = alloc_stack $Float, var, name "y"
// CHECK: [NONE]   %4 = metatype $@thin Float.Type
// CHECK: [NONE]   %5 = metatype $@thin Float.Type
// CHECK: [NONE]   %6 = integer_literal $Builtin.IntLiteral, 1
// CHECK: [NONE]   // function_ref Float.init(_builtinIntegerLiteral:)
// CHECK:   %7 = function_ref @$sSf22_builtinIntegerLiteralSfBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Float.Type) -> Float
// CHECK: [USEFUL]   %8 = apply %7(%6, %5) : $@convention(method) (Builtin.IntLiteral, @thin Float.Type) -> Float // user: %10
// CHECK: [NONE]   // function_ref static Float.+ infix(_:_:)
// CHECK:   %9 = function_ref @$sSf1poiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %10 = apply %9(%0, %8, %4) : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK: [ACTIVE]   %11 = begin_access [modify] [static] %2 : $*Float
// CHECK: [ACTIVE]   %14 = begin_access [read] [static] %2 : $*Float
// CHECK: [ACTIVE]   %15 = load %14 : $*Float
