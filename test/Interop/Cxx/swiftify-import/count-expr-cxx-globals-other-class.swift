// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t%{fs-sep}test.swift -verify -eager-macro-checking

// Check that a count expression can refer to a static data member of a class
// that hasn't been imported yet. Getting the member's Swift type must not
// import the member: that first imports its class, which imports the member
// too, so the member would be imported twice.
//
// This test only checks that importing `S::f` doesn't crash and that `S::f`
// gets a wrapper. count-expr-cxx-globals-wrong-name.swift checks the
// expansion. `-typecheck` doesn't type-check the wrapper's body, which doesn't
// compile because it refers to `T::kCount` as just `kCount`
// (https://github.com/swiftlang/swift/issues/94220).

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

struct T {
  static const size_t kCount;
};

struct S {
  void f(const int * __counted_by(T::kCount + n) p, size_t n);
};

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

// Only `S` is used here, so `T` hasn't been imported yet when `S::f` is. The
// original function takes an `UnsafePointer`, so passing an
// `UnsafeBufferPointer` calls the wrapper.
func callF(_ s: inout S, _ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe s.f(p, n)
}
