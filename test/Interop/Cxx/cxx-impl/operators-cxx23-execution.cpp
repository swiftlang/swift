// Executable end-to-end test: C++ uses C++23 operators implemented in Swift.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -std=c++23 \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/operators-cxx23-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -Xcc -std=c++23 \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name OperatorsCxx23ExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/operators-cxx23-execution-main.o \
// RUN:   %S/Inputs/operators-cxx23-execution.swift \
// RUN:   -o %t/operators-cxx23-execution
// RUN: %target-codesign %t/operators-cxx23-execution
// RUN: %target-run %t/operators-cxx23-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "operators-cxx23.h"

int main() {
  Grid g{10};

  printf("first=%d one=%d two=%d\n", g[], g[7], g[1, 2]);
  // CHECK: first=10 one=7 two=12

  printf("double=%g\n", g[1.5, 2.5]);
  // CHECK: double=17.5

  // The reference result refers to the receiver.
  int &cell = g[0, 0, 0];
  cell = 42;
  printf("cell=%d same=%d\n", g.width, &cell == &g.width);
  // CHECK: cell=42 same=1

  StaticGrid s;
  printf("static=%d %d direct=%d\n", s[3], s[1, 2],
         StaticGrid::operator[](4));
  // CHECK: static=6 12 direct=8

  int swiftResult = swiftCallsSubscripts(g);
  printf("swiftCallsSubscripts=%d width=%d\n", swiftResult, g.width);
  // CHECK: swiftCallsSubscripts=187513 width=3
  return 0;
}
