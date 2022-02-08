// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -Xllvm -sil-print-after=OSLogOptimization -o /dev/null 2>&1 | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
//
// REQUIRES: VENDOR=apple
// REQUIRES: stdlib_static_print

// Tests for the OSLogOptimization pass that performs compile-time analysis
// and optimization of the new os log APIs (and in this case static vprintf).
// The tests here check whether specific compile-time constants such as the
// format string are literals after the mandatory pipeline.

import OSLogTestHelper
import Foundation

// CHECK-LABEL: @${{.*}}testSimpleInterpolationyy
func testSimpleInterpolation() {
  let x = "World"
  print("Hello \(5) \(x)")
  // Match the format string. For now we don't expect all allocations to be avoided
  // CHECK: string_literal utf8 "Hello %ld %s"
}
