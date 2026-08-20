// RUN: %target-swift-frontend %s -emit-ir -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-frontend %s -O -emit-ir -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-OPT
// UNSUPPORTED: CPU=wasm32, OS=linux-androideabi

// CHECK: define{{.*}}swiftcc void @swapPointers({{.*}}noalias{{.*}},{{.*}}noalias{{.*}})
@_silgen_name("swapPointers")
public func swapPointers<T>(_ lhs: inout UnsafePointer<T>, _ rhs: inout UnsafePointer<T>) {}

// CHECK-OPT-LABEL: define{{.*}}swiftcc void @"$s13inout_noalias6rotateyys11InlineArrayVy$63_SdGz_AEzS2dtF"(ptr noalias captures(none) dereferenceable(512) %0, ptr noalias captures(none) dereferenceable(512) %1, double %2, double %3) {{.*}} {
// CHECK-OPT-NOT:     %found.conflict
// CHECK-OPT-NOT:     scalar.ph
// CHECK-OPT:         vector.body
// CHECK-OPT:       ret void
public func rotate(_ x: inout InlineArray<64, Double>,
                   _ y: inout InlineArray<64, Double>,
                   _ c: Double, _ s: Double) {
  for i in x.indices {
    let xi = x[i], yi = y[i]
    x[i] = c * xi + s * yi
    y[i] = c * yi - s * xi
  }
}
