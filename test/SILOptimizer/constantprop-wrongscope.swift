// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I \
// RUN:   %S/../ClangImporter/Inputs/custom-modules -emit-sil -o /dev/null \
// RUN:   -primary-file %s -Xllvm -sil-print-debuginfo -module-name patatino \
// RUN:   -Onone -Xllvm -sil-print-after=diagnostic-constant-propagation \
// RUN:   -Xllvm -sil-print-only-functions=$S8patatino19indexedSubscripting1b3idx1aySo1BC_SiSo1ACtF \
// RUN:   2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Make sure that the destroy_addr instruction has the same scope of the
// instructions surrounding it.

// CHECK:   %60 = unchecked_take_enum_data_addr %12 : $*Optional<Any>, #Optional.some!enumelt.1, loc {{.*}}:24:13, scope 2
// CHECK:   destroy_addr %7 : $*Any, loc {{.*}}:24:19, scope 2
// CHECK:   dealloc_stack %12 : $*Optional<Any>, loc {{.*}}:24:23, scope 2
// CHECK:   dealloc_stack %7 : $*Any, loc {{.*}}:24:23, scope 2
// CHECK:   dealloc_stack %6 : $*A, loc {{.*}}:24:7, scope 2

import Foundation
func indexedSubscripting(b b: B, idx: Int, a: A) {
  var a2 = b[idx] as! A
}
