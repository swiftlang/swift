// Executable end-to-end test: a C++ program calls each member of the overload
// sets declared in overloads.h whose bodies are provided in Swift via
// `@cxx @implementation`.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/overloads-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name OverloadsExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/overloads-execution-main.o \
// RUN:   %S/Inputs/overloads-execution.swift \
// RUN:   -o %t/overloads-execution
// RUN: %target-codesign %t/overloads-execution
// RUN: %target-run %t/overloads-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "overloads.h"

int main() {
  int x = 41;
  printf("overloadedByType(int)=%d\n", overloadedByType(41));
  // CHECK: overloadedByType(int)=42
  printf("overloadedByType(double)=%.1f\n", overloadedByType(1.5));
  // CHECK: overloadedByType(double)=3.0
  int r = overloadedByType(&x);
  printf("overloadedByType(int*)=%d,%d\n", r, x);
  // CHECK: overloadedByType(int*)=42,42

  printf("overloadedByArityAndType(int)=%d\n", overloadedByArityAndType(41));
  // CHECK: overloadedByArityAndType(int)=42
  printf("overloadedByArityAndType(double)=%.1f\n",
         overloadedByArityAndType(1.5));
  // CHECK: overloadedByArityAndType(double)=3.0
  printf("overloadedByArityAndType(int,int)=%d\n",
         overloadedByArityAndType(40, 2));
  // CHECK: overloadedByArityAndType(int,int)=42

  printf("renamedOverload(int)=%d\n", renamedOverload(41));
  // CHECK: renamedOverload(int)=42
  printf("renamedOverload(double)=%.1f\n", renamedOverload(1.5));
  // CHECK: renamedOverload(double)=3.0

  return 0;
}
