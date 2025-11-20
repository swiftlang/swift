// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.0-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-PRESENT %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-5.10-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-SUPPRESSED %s

// REQUIRES: OS=macosx
// UNSUPPORTED: CPU=arm64e

// Make sure that a dangling-based type description involving typed throws is
// only used when deploying to runtimes that support it.

// Closure capture metadata:

enum MyError: Error {
  case fail
}

// CHECK-LABEL: @"\01l__swift5_reflection_descriptor" = private constant
// CHECK-PRESENT-SAME: ptr @"symbolic Si_____Ieghdzo_ 32reflection_metadata_typed_throws7MyErrorO"
// CHECK-SUPPRESSED-SAME: ptr @"symbolic Si_____Ieghdzo_ 32reflection_metadata_typed_throws7MyErrorO"
// CHECK-LABEL: @metadata = private constant %swift.full_boxmetadata { {{.*}}, ptr @"\01l__swift5_reflection_descriptor" }, align
func makeClosure(fn: @escaping @Sendable () throws(MyError) -> Int) -> (() throws(MyError)  -> Int) {
  return { try fn() + 1 }
}

// Struct field metadata:

public struct MyStruct {
  let fn: () throws (MyError) -> ()
}

// CHECK-LABEL: @"$s32reflection_metadata_typed_throws8MyStructVMF" = internal constant
// CHECK-PRESENT-SAME: ptr @"symbolic yy_____YKc 32reflection_metadata_typed_throws7MyErrorO"
// CHECK-SUPPRESSED-SAME: ptr @"get_type_metadata yy32reflection_metadata_typed_throws7MyErrorOYKc.1"
