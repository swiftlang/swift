// RUN: %target-swift-frontend -O -module-name builtin_freeze -enable-experimental-feature BuiltinModule -primary-file %s -emit-ir -o - | %FileCheck %s --check-prefix=CHECK

// REQUIRES: swift_feature_BuiltinModule

import Builtin

func fptosi(_ x: Float) -> Int32 {
  Int32(Builtin.fptosi_FPIEEE32_Int32(x._value))
  // CHECK: fptosi float %{{.+}} to i32
}

func fptosiWithFreeze(_ x: Float) -> Int32 {
  Int32(Builtin.freeze_Int32(Builtin.fptosi_FPIEEE32_Int32(x._value)))
  // CHECK: fptosi float %{{.+}} to i32
  // CHECK-NEXT: freeze i32 %{{.+}}
}

func yuck() -> Int32 {
  fptosi(0x1.0p32)
  // CHECK: poison
}

// CHECK: define{{.*}} swiftcc{{.*}} i32 @"$s14builtin_freeze3yums5Int32VyF"()
func yum() -> Int32 {
  fptosiWithFreeze(0x1.0p32)
  // CHECK-NOT: poison
}
// CHECK: }

func fptosi(_ x: SIMD2<Float>) -> SIMD2<Int32> {
  let maybePoison = Builtin.fptosi_Vec2xFPIEEE32_Vec2xInt32(x._storage._value)
  var result = SIMD2<Int32>()
  result._storage._value = maybePoison
  return result
  // CHECK: fptosi <2 x float> %{{.+}} to <2 x i32>
}

func fptosiWithFreeze(_ x: SIMD2<Float>) -> SIMD2<Int32> {
  let maybePoison = Builtin.fptosi_Vec2xFPIEEE32_Vec2xInt32(x._storage._value)
  let frozen = Builtin.freeze_Vec2xInt32(maybePoison)
  var result = SIMD2<Int32>()
  result._storage._value = frozen
  return result
  // CHECK: fptosi <2 x float> %{{.+}} to <2 x i32>
  // CHECK-NEXT: freeze <2 x i32> %{{.+}}
}

func doubleYuck(_ x: SIMD2<Float>) -> SIMD2<Int32> {
  fptosi(SIMD2<Float>(repeating: 0x1.0p32))
  // CHECK: poison
}

func DoubleYum(_ x: SIMD2<Float>) -> SIMD2<Int32> {
  fptosiWithFreeze(SIMD2<Float>(repeating: 0x1.0p32))
  // CHECK-NOT: poison
}
