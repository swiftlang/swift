// RUN: %target-swift-frontend -import-objc-header %S/Inputs/pass_object_size.h -primary-file %s -emit-ir | %FileCheck %s

// Calls to C functions with __attribute__((pass_object_size)) parameters must
// pass the implicit size argument that Clang's ABI requires, immediately after
// the pointer it describes. The value matches what Clang emits for an opaque
// pointer: llvm.objectsize(p, min, /*nullunknown=*/true, dynamic), where `min`
// is set for pass_object_size types 2 and 3.

// REQUIRES: PTRSIZE=64

// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size7testMaxyySpys5Int32VGF"
func testMax(_ p: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: call void @pos_max(ptr %0, i64 [[SIZE]])
  pos_max(p)
}

// Type 1 is a sub-object query, but llvm.objectsize still gets `min = false`.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size7testSubyySpys5Int32VGF"
func testSub(_ p: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: call void @pos_sub(ptr %0, i64 [[SIZE]])
  pos_sub(p)
}

// Type 2 is a minimum query, so `min = true`.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size7testMinyySpys5Int32VGF"
func testMin(_ p: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 true, i1 true, i1 false)
  // CHECK: call void @pos_min(ptr %0, i64 [[SIZE]])
  pos_min(p)
}

// pass_dynamic_object_size sets the dynamic bit.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size11testDynamicyySpys5Int32VGF"
func testDynamic(_ p: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 true)
  // CHECK: call void @pos_dyn(ptr %0, i64 [[SIZE]])
  pos_dyn(p)
}

// Each annotated parameter gets its own size argument, in order, and each one
// answers its own query.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size7testTwoyySpys5Int32VG_AEtF"
func testTwo(_ p: UnsafeMutablePointer<CInt>, _ q: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE0:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: [[SIZE1:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %1, i1 true, i1 true, i1 false)
  // CHECK: call void @pos_two(ptr %0, i64 [[SIZE0]], ptr %1, i64 [[SIZE1]])
  pos_two(p, q)
}

// The size argument is interleaved, not appended.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size9testMixedyySpys5Int32VG_A2EtF"
func testMixed(_ a: UnsafeMutablePointer<CInt>, _ p: UnsafeMutablePointer<CInt>,
               _ b: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %1, i1 false, i1 true, i1 false)
  // CHECK: call void @pos_mixed(ptr %0, ptr %1, i64 [[SIZE]], ptr %2)
  pos_mixed(a, p, b)
}

// A non-void result and a trailing unannotated parameter.
// CHECK-LABEL: define hidden swiftcc i32 @"$s16pass_object_size10testResultys5Int32VSpys4Int8VGF"
func testResult(_ p: UnsafeMutablePointer<CChar>) -> CInt {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: call i32 @pos_result(ptr %0, i64 [[SIZE]], i32 7)
  return pos_result(p, 7)
}

// An indirectly-passed struct before the annotated parameter: the Clang
// argument index and the SIL parameter index diverge here.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size7testBigyySo03PosE6Structa_Spys5Int32VGtF"
func testBig(_ s: PosBigStruct, _ p: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %1, i1 false, i1 true, i1 false)
  // CHECK: call void @pos_big(ptr {{[^,]*}}, ptr %1, i64 [[SIZE]])
  pos_big(s, p)
}

// Control: an unannotated function gains nothing.
// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size9testPlainyySpys5Int32VGF"
func testPlain(_ p: UnsafeMutablePointer<CInt>) {
  // CHECK-NOT: llvm.objectsize
  // CHECK: call void @pos_plain(ptr %0)
  pos_plain(p)
}

// Referencing the function as a value goes through a foreign-to-native thunk.
// The thunk only has an opaque pointer to go on, so the size it computes is
// "unknown" -- but it must still pass one.
func testAsValue() -> (UnsafeMutablePointer<CInt>?) -> Void {
  return pos_max
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$sSo7pos_maxyySpys5Int32VGSgFTO"
// CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
// CHECK: call void @pos_max(ptr %0, i64 [[SIZE]])
