// Executable end-to-end test: a C++ program calls namespace members declared
// in namespaces.h whose bodies are provided in Swift via
// `@cxx @implementation` functions in extensions of the imported namespaces.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/namespaces-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name NamespacesExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/namespaces-execution-main.o \
// RUN:   %S/Inputs/namespaces-execution.swift \
// RUN:   -o %t/namespaces-execution
// RUN: %target-codesign %t/namespaces-execution
// RUN: %target-run %t/namespaces-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "namespaces.h"

int main() {
  printf("add=%d\n", Outer::add(20, 22));
  // CHECK: add=42

  printf("renamed=%d\n", Outer::renamedTarget(21));
  // CHECK: renamed=42

  printf("overloadedByArity1=%d\n", Outer::overloadedByArity(41));
  // CHECK: overloadedByArity1=42

  printf("overloadedByArity2=%d\n", Outer::overloadedByArity(67, 2));
  // CHECK: overloadedByArity2=69

  printf("nestedFunc=%d\n", Outer::Inner::nestedFunc(-42));
  // CHECK: nestedFunc=42

  return 0;
}
