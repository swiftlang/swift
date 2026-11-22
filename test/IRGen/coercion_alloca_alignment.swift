// RUN: %target-swift-frontend -Onone -emit-ir %s | %FileCheck %s

// REQUIRES: CPU=x86_64 || CPU=arm64 || CPU=aarch64

// A value whose Swift type is more aligned than the LLVM types of its native
// explosion is coerced through a temporary alloca. When IRGen forwards that
// temporary as the source of a copy (the argument and the direct result
// peepholes in visitStoreInst), the copy claims the alignment of the Swift
// type, so the temporary must be at least that aligned. Otherwise x86_64
// lowers the copy to movaps on an 8-byte aligned stack slot and crashes.

enum Action { case load, clear(SIMD4<Float>), dontCare }

// 16-byte aligned (SIMD4<Float>), returned and passed as { i32, i64, i64, i16 }.
struct Row {
  var a: Bool?
  var b: Bool?
  var c: Bool?
  var action: Action
  var d: Bool
}

@inline(never)
func makeRow(_ v: SIMD4<Float>) -> Row {
  return Row(a: nil, b: true, c: false, action: .clear(v), d: true)
}

@inline(never)
func useRow(_ r: Row) {}

// Direct result stored into a local.
// CHECK-LABEL: define {{.*}}swiftcc void @"$s{{.*}}10testResult{{.*}}"
// CHECK:         %temp-coercion.coerced = alloca {{.*}}, align 16
// CHECK:         call swiftcc {{.*}} @"$s{{.*}}7makeRow{{.*}}"
// CHECK:         call void @llvm.memcpy.p0.p0.i64(ptr align 16 %{{.*}}, ptr align 16 %temp-coercion.coerced, i64 {{[0-9]+}}, i1 false)
// CHECK:       ret void
public func testResult(_ v: SIMD4<Float>) {
  var r = makeRow(v)
  useRow(r)
  r.d = false
}

// Argument stored into a local.
// CHECK-LABEL: define {{.*}}swiftcc void @"$s{{.*}}12testArgument{{.*}}"
// CHECK:         %temp-coercion.coerced = alloca {{.*}}, align 16
// CHECK:         call void @llvm.memcpy.p0.p0.i64(ptr align 16 %{{.*}}, ptr align 16 %temp-coercion.coerced, i64 {{[0-9]+}}, i1 false)
// CHECK:       ret void
@inline(never)
func testArgument(_ r: Row) {
  var copy = r
  useRow(copy)
  copy.d = false
}
