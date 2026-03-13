// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Osize -emit-sil -module-name objc_bridged_results %s -import-objc-header %S/Inputs/objc_optional.h | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: rdar114110966

import Foundation

// CHECK-LABEL: sil @$s20objc_bridged_results20testOptionalShortCutyySo4TestCSg_AEtF
// CHECK: bb0
// CHECK: switch_enum
// CHECK: bb1
// CHECK-NOT: switch_enum
// CHECK: apply
// CHECK-NOT: switch_enum
// CHECK: apply
// CHECK-NOT: switch_enum
// CHECK: br bb2
// CHECK: bb2:
// CHECK: return
// CHECK: bb3:
// CHECK: br bb2
public func testOptionalShortCut(_ t: Optional<Test>, _ v: Optional<Test>) {
  t?.other = v?.other?.other
}

public class InClass {
  var t: Test?
  var v: Test?

// CHECK-LABEL: sil {{.*}}@$s20objc_bridged_results7InClassC20testOptionalShortCutyyF
// CHECK: bb0
// CHECK:  switch_enum
// CHECK: bb1
// CHECK:  apply
// CHECK:  apply
// CHECK:  br bb2
// CHECK: bb2:
// CHECK:  return
// CHECK: bb3:
// CHECK:  br bb2
  public func testOptionalShortCut() {
    t?.other = v?.other
  }

// CHECK-LABEL: sil {{.*}}@$s20objc_bridged_results7InClassC21testOptionalShortCut2yyF
// CHECK: bb0
// CHECK:  switch_enum
// CHECK: bb1
// CHECK:  br bb5
// CHECK: bb2({{.*}}):
// CHECK:  apply
// CHECK:  switch_enum
// CHECK: bb3:
// CHECK:  br bb5
// CHECK: bb4({{.*}}):
// CHECK:  objc_method
// CHECK:  apply
// CHECK:  objc_method
// CHECK:  apply
// CHECK:  objc_method
// CHECK:  apply
// CHECK:  br bb5
// CHECK: bb5:
// CHECK:  return
  public func testOptionalShortCut2() {
    t?.other?.other = v?.other?.other
  }
}
