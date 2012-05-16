// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

class Ty0 {
  var x : Int
  var y : Int

  func f() -> Int { return x }
  static func g() -> Ty0 { return new Ty0 }
}

// FIXME: We generate really bad code for the function f.
// CHECK: define i64 @_TN5class3Ty01ffS0_FT_NSs5Int64(%_T5class3Ty0* %this) {
// CHECK: call {{.*}} @swift_retain
// CHECK: call {{.*}} @swift_release
// CHECK: call {{.*}} @swift_release

// CHECK: define %_T5class3Ty0* @_TN5class3Ty01gFT_S0_() {
// CHECK: [[RETVAL:%.*]] = alloca %_T5class3Ty0*, align 8
// CHECK-NEXT: [[NEW:%.*]] = call %swift.refcounted* @swift_alloc(%swift.heapmetadata* @metadata, i64 32, i64 8) nounwind
// CHECK-NEXT: [[BITCAST:%.*]] = bitcast %swift.refcounted* [[NEW]] to %_T5class3Ty0*
// CHECK-NEXT: store %_T5class3Ty0* [[BITCAST]], %_T5class3Ty0** [[RETVAL]], align 8
