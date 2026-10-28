// Executable end-to-end test: a C++ program calls functions declared in
// functions.h whose bodies are provided in Swift via `@cxx @implementation`.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -Wno-nullability-completeness \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/functions-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name FunctionsExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -Xcc -Wno-nullability-completeness \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/functions-execution-main.o \
// RUN:   %S/Inputs/functions-execution.swift \
// RUN:   -o %t/functions-execution
// RUN: %target-codesign %t/functions-execution
// RUN: %target-run %t/functions-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdint.h>
#include <stdio.h>

#include "functions.h"

int main() {
  printf("returnsInt=%d\n", returnsInt());
  // CHECK: returnsInt=42

  printf("takesInt64=%d\n", (int)takesInt64(64));
  // CHECK: takesInt64=64

  unsigned p = (unsigned)(uintptr_t)returnsPtrToInt();
  printf("returnsPtrToInt=%u\n", p);
  // CHECK: returnsPtrToInt=0

  int a = 1, b = 2;
  swapInts(&a, &b);
  printf("swapInts=%d,%d\n", a, b);
  // CHECK: swapInts=2,1

  TrivialStruct s = returnsTrivialStruct();
  printf("returnsTrivialStruct=%d,%d\n", s.x, s.y);
  // CHECK: returnsTrivialStruct=7,9

  printf("overloadedByArity1=%d\n", overloadedByArity(41));
  // CHECK: overloadedByArity1=42
  printf("overloadedByArity2=%d\n", overloadedByArity(67, 2));
  // CHECK: overloadedByArity2=69

  printf("withDefaultArg=%d\n", withDefaultArg(32));
  // CHECK: withDefaultArg=42
  printf("withDefaultArg=%d\n", withDefaultArg(67, 2));
  // CHECK: withDefaultArg=69

  printf("externCFunc=%d\n", externCFunc(42));
  // CHECK: externCFunc=-42

  printf("foo=%d\n", foo(21));
  // CHECK: foo=42

  return 0;
}
