// RUN: %swift -target x86_64-apple-darwin10 -emit-ir %s | FileCheck %s

// Make sure we use an intrinsic for functions such as sqrt
// CHECK: tail call float @llvm.sqrt.f32
func(f : Float) -> Float {
  return sqrt(f)
}

// CHECK: tail call float @llvm.sqrt.f64
func(f : Double) -> Double {
  return sqrt(f)
}
