// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Embedded -O -c -o %t/main.o -cxx-interoperability-mode=default
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

@_expose(Cxx)
func f1() {
  print("OK!")
}

f1()

// CHECK: OK!
