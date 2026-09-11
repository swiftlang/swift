// Executable end-to-end test: a C++ program calls methods declared in
// methods.h whose bodies are provided in Swift via `@cxx @implementation`
// methods in extensions of the imported structs.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/methods-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name MethodsExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/methods-execution-main.o \
// RUN:   %S/Inputs/methods-execution.swift \
// RUN:   -o %t/methods-execution
// RUN: %target-codesign %t/methods-execution
// RUN: %target-run %t/methods-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "methods.h"

int main() {
  Counter counter = Counter::make(40);
  printf("make=%d\n", counter.value);
  // CHECK: make=40

  counter.add(2);
  printf("add=%d\n", counter.value);
  // CHECK: add=42

  printf("get=%d\n", counter.get());
  // CHECK: get=42

  printf("overloadedByArity=%d %d\n", counter.overloadedByArity(),
         counter.overloadedByArity(8));
  // CHECK: overloadedByArity=42 50

  printf("renamedTarget=%d\n", counter.renamedTarget());
  // CHECK: renamedTarget=84

  Pair pair{10};
  const Pair &constPair = pair;
  int adjustConstRes = constPair.adjust(5);
  printf("adjustConst=%d value=%d\n", adjustConstRes, pair.value);
  // CHECK: adjustConst=15 value=10

  int adjustNonConstRes = pair.adjust(5);
  printf("adjustNonConst=%d value=%d\n", adjustNonConstRes, pair.value);
  // CHECK: adjustNonConst=15 value=15

  int adjustTwoRes = pair.adjust(1, 2);
  printf("adjustTwo=%d value=%d\n", adjustTwoRes, pair.value);
  // CHECK: adjustTwo=18 value=18

  Holder holder{7};
  Triple spread = holder.spread(3);
  printf("spread=%ld %ld %ld\n", spread.a, spread.b, spread.c);
  // CHECK: spread=7 3 10

  Triple triple = Holder::makeTriple(20);
  printf("makeTriple=%ld %ld %ld\n", triple.a, triple.b, triple.c);
  // CHECK: makeTriple=20 21 22

  NonTrivialReceiver receiver;
  receiver.write(9);
  printf("nonTrivialReceiver=%d\n", receiver.read());
  // CHECK: nonTrivialReceiver=9

  return 0;
}
