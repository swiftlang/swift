// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I \
// RUN:   %S/../ClangImporter/Inputs/custom-modules -Xllvm -sil-print-types -emit-sil -o /dev/null \
// RUN:   -primary-file %s -Xllvm -sil-print-types -Xllvm -sil-print-debuginfo -module-name patatino \
// RUN:   -Onone -Xllvm -sil-print-types -Xllvm -sil-print-after=diagnostic-constant-propagation \
// RUN:   2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Make sure that the destroy_addr instruction has the same scope of the
// instructions surrounding it.

// CHECK:   destroy_addr %7 : $*Any, loc {{.*}}:22:19, scope 3
// CHECK:   dealloc_stack %12 : $*Optional<Any>, loc {{.*}}:22:23, scope 3
// CHECK:   dealloc_stack %7 : $*Any, loc {{.*}}:22:23, scope 3
// CHECK:   dealloc_stack %6 : $*A, loc {{.*}}:22:7, scope 3

import Foundation
func indexedSubscripting(b b: B, idx: Int, a: A) {
  var a2 = b[idx] as! A
}
