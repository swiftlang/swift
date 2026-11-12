// A `@cxx @implementation` function borrows a foreign reference type parameter
// and `self` from its C++ caller, so it does not retain them. A `free()`
// override that calls `super.free()` therefore does not touch the object after
// the base class has freed it.

// RUN: %empty-directory(%t)

// RUN: %target-interop-build-clangxx -c %s -I %S/Inputs -o %t/main.o

// RUN: %target-interop-build-swift \
// RUN:   -Onone \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -module-name ForeignReferenceFreeExecution \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/main.o \
// RUN:   %S/Inputs/foreign-reference-free-execution.swift \
// RUN:   -o %t/onone
// RUN: %target-codesign %t/onone
// RUN: %target-run %t/onone | %FileCheck %s

// RUN: %target-interop-build-swift \
// RUN:   -O \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -module-name ForeignReferenceFreeExecution \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/main.o \
// RUN:   %S/Inputs/foreign-reference-free-execution.swift \
// RUN:   -o %t/o
// RUN: %target-codesign %t/o
// RUN: %target-run %t/o | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: stdlib_5_8_runtime
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "foreign-reference-free.h"

void Object::anchor() const {}
void Derived::anchor() const {}

void Object::free() {
  printf("Object::free\n");
  freed = true;
}

void retainObject(Object *object) {
  printf("retain %d->%d%s\n", object->refs, object->refs + 1,
         object->freed ? " after free" : "");
  ++object->refs;
}

void releaseObject(Object *object) {
  printf("release %d->%d%s\n", object->refs, object->refs - 1,
         object->freed ? " after free" : "");
  if (--object->refs == 0 && !object->freed)
    object->free();
}

void note(const char *message) { printf("%s\n", message); }
void noteRefs(int refs) { printf("refs=%d\n", refs); }

int main() {
  Derived *derived = new Derived;
  printf("start\n");
  // CHECK:      start

  inspect(derived);
  // CHECK-NEXT: inspect
  // CHECK-NEXT: refs=1

  // The last reference: calls Derived::free(), in Swift.
  releaseObject(derived);
  // CHECK-NEXT: release 1->0
  // CHECK-NEXT: Derived::free
  // CHECK-NEXT: Object::free

  printf("done\n");
  // CHECK-NEXT: done

  delete derived;
  return 0;
}
