// Executable end-to-end test: a C++ program calls functions declared in
// references.h whose bodies are provided in Swift via `@cxx @implementation`.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/references-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name ReferencesExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/references-execution-main.o \
// RUN:   %S/Inputs/references-execution.swift \
// RUN:   -o %t/references-execution
// RUN: %target-codesign %t/references-execution
// RUN: %target-run %t/references-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "references.h"

int referencesGlobal = 0;

int main() {
  int x = 41;
  int r1 = addOne(x);
  printf("addOne: %d %d\n", x, r1);
  // CHECK: addOne: 42 42

  int a = 1, b = 2;
  swapRefs(a, b);
  printf("swapRefs: %d %d\n", a, b);
  // CHECK: swapRefs: 2 1

  const int c = 5;
  int r2 = readConstRef(c);
  printf("readConstRef: %d\n", r2);
  // CHECK: readConstRef: 50

  int &m = mutableRefReturn();
  int before = m;
  m = 100;
  const int &k = constRefReturn();
  int after = k;
  printf("refReturns: %d %d\n", before, after);
  // CHECK: refReturns: 7 100

  int *&slot = refToPtrReturn();
  int through = *slot;
  printf("refToPtr: %d\n", through);
  // CHECK: refToPtr: 100

  int *p = nullptr;
  reseatPtr(p);
  int reseated = (p != nullptr) ? *p : -1;
  printf("reseatPtr: %d\n", reseated);
  // CHECK: reseatPtr: 100

  int v = 1;
  refOverload(v); // Binds int &.
  printf("refOverloadRef: %d\n", v);
  // CHECK: refOverloadRef: 101

  refOverload(v + 0); // A prvalue binds const int &.
  printf("refOverloadConstRef: %d\n", referencesGlobal);
  // CHECK: refOverloadConstRef: 101

  refOverload(&v); // Binds int *.
  printf("refOverloadPtr: %d\n", v);
  // CHECK: refOverloadPtr: 1101

  Accumulator acc{5};
  int t = 10;
  int r3 = acc.addTo(t);
  printf("addTo: %d %d\n", t, r3);
  // CHECK: addTo: 15 15

  Accumulator acc2{5};
  bumpTotal(acc2);
  int total = readTotal(acc2);
  printf("structRefs: %d %d\n", acc2.total, total);
  // CHECK: structRefs: 6 6

  return 0;
}
