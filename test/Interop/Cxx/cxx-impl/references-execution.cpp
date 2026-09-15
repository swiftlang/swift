// Executable end-to-end test: a C++ program calls functions declared in
// references.h whose bodies are provided in Swift via `@cxx @implementation`.
// The C++ side passes references, which C++ permits and the Swift
// implementations must tolerate.

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

bool operator==(const PointerHolder &h, int *const &p) { return h.p == p; }

int main() {
  int x = 41;
  int r1 = addOne(x);
  printf("addOne: %d %d\n", x, r1);
  // CHECK: addOne: 42 42

  int a = 1, b = 2;
  swapRefs(a, b);
  printf("swapRefs: %d %d\n", a, b);
  // CHECK: swapRefs: 2 1

  swapRefs(a, a); // A self-swap must be harmless.
  printf("selfSwap: %d\n", a);
  // CHECK: selfSwap: 2

  // Aliasing references: the body writes 1 through `a`, 2 through `b`, and
  // returns `a`'s referent, which the write through `b` just changed.
  int o = 0;
  int r2 = observe(o, o);
  printf("observe: %d %d\n", o, r2);
  // CHECK: observe: 2 2

  // The reference may alias a global the body also uses.
  int r3 = observeGlobal(referencesGlobal);
  printf("observeGlobal: %d %d\n", referencesGlobal, r3);
  // CHECK: observeGlobal: 7 7

  const int c = 5;
  int r4 = readConstRef(c);
  printf("readConstRef: %d\n", r4);
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

  Accumulator acc{5};
  int t = 10;
  int r5 = acc.addTo(t);
  printf("addTo: %d %d\n", t, r5);
  // CHECK: addTo: 15 15

  Accumulator acc2{5};
  bumpTotal(acc2);
  int total = readTotal(acc2);
  identityRef(acc2).total = 9;
  printf("structRefs: %d %d\n", total, acc2.total);
  // CHECK: structRefs: 6 9

  // The Swift body compares through the imported operator, whose const
  // reference to a pointer must receive the pointer's address.
  int target = 0;
  PointerHolder holder{&target};
  bool same = holderMatches(holder, &target);
  bool other = holderMatches(holder, &x);
  printf("holderMatches: %d %d\n", same, other);
  // CHECK: holderMatches: 1 0

  return 0;
}
