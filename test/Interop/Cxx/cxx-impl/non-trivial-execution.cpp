// Executable end-to-end test: a C++ program calls functions and methods
// declared in non-trivial.h that take and return non-trivial C++ classes,
// whose bodies are provided in Swift via `@cxx @implementation`, and observes
// their results together with the classes' construction and destruction
// counts.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/non-trivial-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -module-name NonTrivialExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/non-trivial-execution-main.o \
// RUN:   %S/Inputs/non-trivial-execution.swift \
// RUN:   -o %t/non-trivial-execution
// RUN: %target-codesign %t/non-trivial-execution
// RUN: %target-run %t/non-trivial-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "non-trivial.h"

int Tracked::liveCount = 0;
int Tracked::copyCount = 0;
Tracked::Tracked(int v) : value(v) { ++liveCount; }
Tracked::Tracked(const Tracked &other) : value(other.value) {
  ++liveCount;
  ++copyCount;
}
Tracked::~Tracked() {
  --liveCount;
  value = -1;
}

int Movable::liveCount = 0;
Movable::Movable(int v) : value(v) { ++liveCount; }
Movable::Movable(const Movable &other) : value(other.value) { ++liveCount; }
Movable::Movable(Movable &&other) : value(other.value) {
  other.value = -1;
  ++liveCount;
}
Movable::~Movable() { --liveCount; }

int MoveOnly::liveCount = 0;
MoveOnly::MoveOnly(int v) : value(v) { ++liveCount; }
MoveOnly::MoveOnly(MoveOnly &&other) : value(other.value) {
  other.value = -1;
  ++liveCount;
}
MoveOnly::~MoveOnly() { --liveCount; }

Polymorphic::Polymorphic(int v) : value(v) {}
Polymorphic::Polymorphic(const Polymorphic &other) : value(other.value) {}
Polymorphic::~Polymorphic() {}
int Polymorphic::tag() const { return 1000; }

int main() {
  {
    Tracked t(5);
    int result = takesTracked(t);
    // CHECK: takesTracked: 5 copies=1
    printf("takesTracked: %d copies=%d\n", result, Tracked::copyCount);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);
  Tracked::copyCount = 0;

  {
    Tracked a(1), b(2);
    int result = takesTwoTracked(a, b);
    // CHECK-NEXT: takesTwoTracked: 3 copies=2
    printf("takesTwoTracked: %d copies=%d\n", result, Tracked::copyCount);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);
  Tracked::copyCount = 0;

  {
    Tracked t(7);
    int result = copiesTracked(t);
    // CHECK-NEXT: copiesTracked: 107 original=7
    printf("copiesTracked: %d original=%d\n", result, t.value);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);
  Tracked::copyCount = 0;

  {
    Tracked t = returnsTracked(9);
    // CHECK-NEXT: returnsTracked: 9
    printf("returnsTracked: %d\n", t.value);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);

  {
    Tracked t(3);
    Tracked r = passesThroughTracked(t);
    // CHECK-NEXT: passesThroughTracked: 3 original=3
    printf("passesThroughTracked: %d original=%d\n", r.value, t.value);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);

  {
    int result = passesThroughTracked(Tracked(11)).value;
    // CHECK-NEXT: passesThroughTracked(temporary): 11
    printf("passesThroughTracked(temporary): %d\n", result);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);

  {
    Movable m(6);
    int fromCopy = takesMovable(m);
    int fromMove = takesMovable(static_cast<Movable &&>(m));
    // CHECK-NEXT: takesMovable: 6 6 moved-from=-1
    printf("takesMovable: %d %d moved-from=%d\n", fromCopy, fromMove, m.value);
    Movable r = returnsMovable(8);
    // CHECK-NEXT: returnsMovable: 8
    printf("returnsMovable: %d\n", r.value);
  }
  // CHECK-NEXT: movable live=0
  printf("movable live=%d\n", Movable::liveCount);

  {
    MoveOnly r = returnsMoveOnly(12);
    // CHECK-NEXT: returnsMoveOnly: 12
    printf("returnsMoveOnly: %d\n", r.value);
  }
  // CHECK-NEXT: move-only live=0
  printf("move-only live=%d\n", MoveOnly::liveCount);

  {
    Polymorphic p(5);
    int result = takesPolymorphic(p);
    // CHECK-NEXT: takesPolymorphic: 1005
    printf("takesPolymorphic: %d\n", result);
  }

  {
    Box box{10};
    Tracked t(4);
    int taken = box.take(t);
    int added = box.add(t);
    // CHECK-NEXT: Box: take=14 add=14 base=14
    printf("Box: take=%d add=%d base=%d\n", taken, added, box.base);
    Tracked produced = box.produce();
    Tracked wrapped = Box::wrap(30);
    // CHECK-NEXT: Box: produce=14 wrap=30
    printf("Box: produce=%d wrap=%d\n", produced.value, wrapped.value);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);
  Tracked::copyCount = 0;

  {
    Tracked a(5), b(9);
    int read = readTracked(a);
    bumpTracked(a);
    int bumped = a.value;
    assignTracked(a, b);
    int assigned = a.value;
    // CHECK-NEXT: references: read=5 bumped=6 assigned=9
    printf("references: read=%d bumped=%d assigned=%d\n", read, bumped, assigned);
  }
  // CHECK-NEXT: live=0
  printf("live=%d\n", Tracked::liveCount);

  return 0;
}
