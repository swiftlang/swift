// RUN: %target-swift-frontend -import-objc-header %S/Inputs/pass_object_size.h -primary-file %s -emit-ir | %FileCheck %s

// When a C overload set differs only in pass_object_size, both candidates
// import into Swift with the same type, so overload resolution is ambiguous
// unless we break the tie the way Clang does: prefer the candidate with the
// most pass_object_size parameters. The mangled Clang names below are the same
// ones Clang itself selects for these calls.

// REQUIRES: PTRSIZE=64

// CHECK-LABEL: define hidden swiftcc i64 @"$s25pass_object_size_overload9testOnaryySiSpys4Int8VGF"
func testOnary(_ p: UnsafeMutablePointer<CChar>) -> Int {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: call i64 @_Z3ovlPcU17pass_object_size0(ptr %0, i64 [[SIZE]])
  return ovl(p)
}

// CHECK-LABEL: define hidden swiftcc i64 @"$s25pass_object_size_overload10testBinaryySiSpys4Int8VG_AEtF"
func testBinary(_ p: UnsafeMutablePointer<CChar>, _ q: UnsafeMutablePointer<CChar>) -> Int {
  // CHECK: [[SIZE0:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: [[SIZE1:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %1, i1 true, i1 true, i1 false)
  // CHECK: call i64 @_Z4ovl2PcU17pass_object_size0S_U17pass_object_size2(ptr %0, i64 [[SIZE0]], ptr %1, i64 [[SIZE1]])
  return ovl2(p, q)
}

// The unannotated overloads are never selected, so they are not even declared.
// CHECK-NOT: declare{{.*}}@_Z3ovlPc(
// CHECK-NOT: declare{{.*}}@_Z4ovl2PcS_(
