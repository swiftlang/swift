// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none -Osize | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none -O | %FileCheck %s

// REQUIRES: swift_feature_Embedded

// Trivial elements get a single memcpy/memmove, no per-element loop.

struct Trivial {
  var x: Builtin.Int64
}

// Disjoint -> memcpy.
// CHECK-LABEL: define {{.*}} @"{{.*}}4copy{{.*}}"
// CHECK-NOT:     loop_body
// CHECK:         @llvm.memcpy
// CHECK:       ret void
public func copy(x: Builtin.RawPointer, y: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(Trivial.self, x, y, count)
}

// May overlap -> memmove.
// CHECK-LABEL: define {{.*}} @"{{.*}}6assign{{.*}}"
// CHECK-NOT:     loop_body
// CHECK:         @llvm.memmove
// CHECK:       ret void
public func assign(x: Builtin.RawPointer, y: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.assignCopyArrayFrontToBack(Trivial.self, x, y, count)
}
