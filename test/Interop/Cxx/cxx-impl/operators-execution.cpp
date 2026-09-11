// Executable end-to-end test: a C++ program uses operators declared in
// operators.h whose bodies are provided in Swift via `@cxx @implementation`.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/operators-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -module-name OperatorsExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/operators-execution-main.o \
// RUN:   %S/Inputs/operators-execution.swift \
// RUN:   -o %t/operators-execution
// RUN: %target-codesign %t/operators-execution
// RUN: %target-run %t/operators-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "operators.h"

// Retains minus releases.
static int liveHandles = 0;

void retainHandle(Handle *) { ++liveHandles; }
void releaseHandle(Handle *) { --liveHandles; }

int main() {
  Vector a{3}, b{4}, c{3};

  printf("equals=%d %d\n", a == b, a == c);
  // CHECK: equals=0 1

  printf("less=%d %d\n", a < b, b < a);
  // CHECK: less=1 0

  Vector plusVector = a + b;
  Vector plusInt = a + 10;
  printf("plus=%d %d\n", plusVector.x, plusInt.x);
  // CHECK: plus=7 13

  Vector negated = -a;
  Vector minus = a - b;
  printf("minus=%d %d\n", negated.x, minus.x);
  // CHECK: minus=-3 -1

  // The compound assignment returns its receiver.
  Vector &plusEqualsRes = (a += b);
  printf("plusEquals=%d same=%d\n", a.x, &plusEqualsRes == &a);
  // CHECK: plusEquals=7 same=1

  printf("element=%d call=%d\n", a[5], a(2));
  // CHECK: element=12 call=14

  Vector &incrementRes = ++a;
  printf("increment=%d same=%d\n", a.x, &incrementRes == &a);
  // CHECK: increment=8 same=1

  Vector postIncrementRes = a++;
  printf("postIncrement=%d %d\n", postIncrementRes.x, a.x);
  // CHECK: postIncrement=8 9

  printf("notEqual=%d %d\n", a != b, b != b);
  // CHECK: notEqual=1 0

  Vector times = a * 2;
  printf("times=%d\n", times.x);
  // CHECK: times=18

  Outer::Point p{1}, q{1}, r{2};
  printf("point=%d %d\n", p == q, p == r);
  // CHECK: point=1 0

  Handle h{5}, k{7};
  int handleEquals = h == k;
  int handleEqualsSelf = h == h;
  int handleLess = h < k;
  printf("handle=%d %d %d live=%d\n", handleEquals, handleEqualsSelf,
         handleLess, liveHandles);
  // CHECK: handle=0 1 1 live=0

  Vector d{3}, e{4};
  printf("swiftCallsOperators=%d\n", swiftCallsOperators(d, e));
  // CHECK: swiftCallsOperators=-582890
  return 0;
}
