// C++ calls, through base class pointers, overrides in foreign reference types
// implemented in Swift, whose `super` calls the base method statically. Runs
// with Base::describe() in C++ and in Swift.

// RUN: %empty-directory(%t)

// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -DBASE_DESCRIBE_IN_CXX \
// RUN:   -o %t/cxx-base-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -module-name ForeignReferenceVirtualExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -D BASE_DESCRIBE_IN_CXX \
// RUN:   -Xlinker %t/cxx-base-main.o \
// RUN:   %S/Inputs/foreign-reference-virtual-execution.swift \
// RUN:   -o %t/cxx-base
// RUN: %target-codesign %t/cxx-base
// RUN: %target-run %t/cxx-base | %FileCheck %s --check-prefixes=CHECK,CXX-BASE

// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/swift-base-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -module-name ForeignReferenceVirtualExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/swift-base-main.o \
// RUN:   %S/Inputs/foreign-reference-virtual-execution.swift \
// RUN:   -o %t/swift-base
// RUN: %target-codesign %t/swift-base
// RUN: %target-run %t/swift-base | %FileCheck %s --check-prefixes=CHECK,SWIFT-BASE

// REQUIRES: executable_test
// REQUIRES: stdlib_5_8_runtime
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>
#include <typeinfo>

#include "foreign-reference-virtual.h"

// The C++ bodies; Swift defines the rest.
#ifdef BASE_DESCRIBE_IN_CXX
int Base::describe() const { return 100 + value; }
#endif
int Base::tag() const { return 7; }
int Base::hide() const { return 42; }
int SecondBase::fromSecond() const { return -1; }
int AbstractBase::abstractAnchor() const { return -1; }

// Retains minus releases.
static int liveBases = 0;
static int liveAbstractBases = 0;

void retainBase(Base *) { ++liveBases; }
void releaseBase(Base *) { --liveBases; }
void retainAbstractBase(AbstractBase *) { ++liveAbstractBases; }
void releaseAbstractBase(AbstractBase *) { --liveAbstractBases; }

// Virtual calls the compiler cannot devirtualize.
__attribute__((noinline)) static int callDescribe(const Base *base) {
  return base->describe();
}
__attribute__((noinline)) static int callTag(const Base *base) {
  return base->tag();
}
__attribute__((noinline)) static int callFromSecond(const SecondBase *second) {
  return second->fromSecond();
}
__attribute__((noinline)) static int callPure(const AbstractBase *base) {
  return base->pure();
}

int main() {
  Base base;
  base.value = 5;
  Derived derived;
  derived.value = 5;
  Leaf leaf;
  leaf.value = 5;
  MultiDerived multi;
  multi.value = 5;
  multi.second = 7;

  int baseDescribe = callDescribe(&base);
  printf("base=%d live=%d\n", baseDescribe, liveBases);
  // CXX-BASE: base=105 live=0
  // SWIFT-BASE: base=205 live=0

  // Derived's body, whose `super` calls Base's without recursing.
  int derivedDescribe = callDescribe(&derived);
  printf("derived=%d live=%d\n", derivedDescribe, liveBases);
  // CXX-BASE: derived=210 live=0
  // SWIFT-BASE: derived=410 live=0

  // Leaf's body calls Derived's, which calls Base's.
  int leafDescribe = callDescribe(&leaf);
  printf("leaf=%d live=%d\n", leafDescribe, liveBases);
  // CXX-BASE: leaf=211 live=0
  // SWIFT-BASE: leaf=411 live=0

  // Derived only inherits tag(), so Leaf's `super` calls Base's.
  int derivedTag = callTag(&derived);
  int leafTag = callTag(&leaf);
  printf("tag=%d %d live=%d\n", derivedTag, leafTag, liveBases);
  // CHECK: tag=7 1007 live=0

  // `super` calls the hidden Base::hide().
  int hide = derived.hide();
  printf("hide=%d live=%d\n", hide, liveBases);
  // CHECK: hide=43 live=0

  // fromSecond() is reached through the this-adjusting thunk.
  int multiDescribe = callDescribe(&multi);
  int fromSecond = callFromSecond(&multi);
  printf("multi=%d fromSecond=%d live=%d\n", multiDescribe, fromSecond,
         liveBases);
  // CXX-BASE: multi=315 fromSecond=12 live=0
  // SWIFT-BASE: multi=615 fromSecond=12 live=0

  // Swift's RTTI serves dynamic_cast, cross-casts, and typeid.
  Base *bases[] = {&base, &derived, &leaf, &multi};
  int isDerived0 = dynamic_cast<Derived *>(bases[0]) != nullptr;
  int isDerived1 = dynamic_cast<Derived *>(bases[1]) != nullptr;
  int isDerived2 = dynamic_cast<Derived *>(bases[2]) != nullptr;
  int isLeaf = typeid(*bases[2]) == typeid(Leaf);
  SecondBase *crossCast = dynamic_cast<SecondBase *>(bases[3]);
  int crossCastOK = crossCast == static_cast<SecondBase *>(&multi);
  printf("isDerived=%d %d %d isLeaf=%d crossCast=%d\n", isDerived0, isDerived1,
         isDerived2, isLeaf, crossCastOK);
  // CHECK: isDerived=0 1 1 isLeaf=1 crossCast=1

  // ConcreteDerived's RTTI comes from Swift, AbstractBase's from C++.
  ConcreteDerived concrete;
  AbstractBase *abstract = &concrete;
  int pure = callPure(abstract);
  int isConcrete = dynamic_cast<ConcreteDerived *>(abstract) == &concrete;
  printf("pure=%d isConcrete=%d live=%d\n", pure, isConcrete,
         liveAbstractBases);
  // CHECK: pure=8 isConcrete=1 live=0

  return 0;
}
