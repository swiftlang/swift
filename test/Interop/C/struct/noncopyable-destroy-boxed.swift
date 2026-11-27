// A noncopyable C struct with a destroy operation that is only held in a box
// (a local or a consuming parameter) used to have no deinit emitted.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sil -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-sil -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -cxx-interoperability-mode=default | %FileCheck %s --check-prefix=SIL
// FIXME: Also run in C mode once C functions returning these structs are no
// longer lowered as autoreleased.
// RUN: %target-build-swift -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -cxx-interoperability-mode=default -o %t%{fs-sep}a.out
// RUN: %target-codesign %t%{fs-sep}a.out
// RUN: %target-run %t%{fs-sep}a.out | %FileCheck %s

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
static inline Owned makeOwned(int value) {
  Owned o = {value};
  return o;
}

//--- test.swift
import Owned

func consume(_ o: consuming Owned) {}

func test() {
  let o = makeOwned(1)
  _ = o.value
  consume(makeOwned(2))
  let p: Owned? = makeOwned(4)
  _ = p != nil
}

test()
// CHECK: freed: 7
print("freed: \(freed)")

// SIL-LABEL: sil shared @$sSo5OwnedVfD : $@convention(method) (@owned Owned) -> ()
// SIL-LABEL: sil_moveonlydeinit Owned {
// SIL: @$sSo5OwnedVfD
