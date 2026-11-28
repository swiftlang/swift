// Consuming a noncopyable C struct with a destroy operation by value must run
// the destroy operation.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -o %t%{fs-sep}c.out
// RUN: %target-codesign %t%{fs-sep}c.out
// RUN: %target-run %t%{fs-sep}c.out | %FileCheck %s
// RUN: %target-build-swift -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -cxx-interoperability-mode=default -o %t%{fs-sep}cxx.out
// RUN: %target-codesign %t%{fs-sep}cxx.out
// RUN: %target-run %t%{fs-sep}cxx.out | %FileCheck %s

// REQUIRES: executable_test

//--- Inputs/module.modulemap
module Owned {
  header "owned.h"
}

//--- Inputs/owned.h
#pragma once
#include "swift/bridging"

typedef struct SWIFT_NONCOPYABLE_WITH_DESTROY(freeOwned) Owned {
  int value;
} Owned;

static int freed = 0;

static inline void freeOwned(Owned o) { freed += o.value; }

//--- test.swift
import Owned

func discard() {
  let o = Owned(value: 1)
  _ = consume o
}

func consumeTemporary() {
  _ = Owned(value: 2)
}

func reassign() {
  var o = Owned(value: 4)
  o = Owned(value: 8)
  _ = consume o
}

func discardOptional() {
  let o: Owned? = Owned(value: 16)
  _ = consume o
  let n: Owned? = nil
  _ = consume n
}

discard()
// CHECK: freed: 1
print("freed: \(freed)")
consumeTemporary()
// CHECK-NEXT: freed: 3
print("freed: \(freed)")
reassign()
// CHECK-NEXT: freed: 15
print("freed: \(freed)")
discardOptional()
// CHECK-NEXT: freed: 31
print("freed: \(freed)")
