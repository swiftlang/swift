// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test~partial.swiftmodule -module-name Test -primary-file %s
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/Test.swiftmodule %t/Test~partial.swiftmodule
// RUN: %target-swift-ide-test -print-module -module-to-print=Test -source-filename=x -I %t | %FileCheck %s

// CHECK-LABEL: func testDefaultArguments(
public func testDefaultArguments(
  // CHECK-SAME: normal: Int = 0
  normal: Int = 0,
  // CHECK-SAME: multiToken: Int = Int.max
  multiToken: Int = Int.max,
  // CHECK-SAME: special: Int = #line
  special: Int = #line
) {}
// CHECK-SAME: )