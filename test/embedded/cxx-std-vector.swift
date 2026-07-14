// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %S/Inputs %s -enable-experimental-feature Embedded -cxx-interoperability-mode=default -wmo -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -lc++ %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import Cxx
import CxxStdlib
import CxxStdVector

let v = makeVector()
for elem in v {
  print(elem)
}
// CHECK: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
