// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -o /dev/null -primary-file %s -Xllvm -sil-print-debuginfo -module-name patatino -Onone -Xllvm -sil-print-after=diagnostic-constant-propagation 2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Make sure that the destroy_addr instruction has the same scope of the
// instructions surrounding it.

import Foundation
func indexedSubscripting(b b: B, idx: Int, a: A) {
  var a2 = b[idx] as! A
  // CHECK:   destroy_addr %7 : $*Any, loc {{.*}}:[[@LINE-1]]:19, scope 2
  // CHECK:   dealloc_stack %12 : $*Optional<Any>, loc {{.*}}:[[@LINE-2]]:23, scope 2
  // CHECK:   dealloc_stack %7 : $*Any, loc {{.*}}:[[@LINE-3]]:23, scope 2
  // CHECK:   dealloc_stack %6 : $*A, loc {{.*}}:[[@LINE-4]]:7, scope 2
}
