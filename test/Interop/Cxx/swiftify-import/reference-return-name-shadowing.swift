// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Cells

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The generated code refers to the stdlib's Ref and Cell, not to C++ types
// that happen to have the same name.
// RUN: %target-swift-frontend -emit-sil -o /dev/null -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Cells \
// RUN:   -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift -verify -eager-macro-checking

//--- Inputs/module.modulemap
module Shadowing {
  header "shadowing.h"
  requires cplusplus
}

//--- Inputs/shadowing.h
#pragma once

struct Cell { int x; };
struct ConstCell { int x; };
template <class T> struct Ref { T t; };
using IntRef = Ref<int>;

inline int &pick(int &a [[clang::lifetimebound]]) { return a; }
inline const int &constPick(const int &a [[clang::lifetimebound]]) { return a; }

//--- test.swift
import Shadowing

@available(SwiftStdlib 6.2, *)
func test(_ x: inout CInt) {
  // 'Ref' is Copyable, so the exclusive access to 'x' lasts until 'r' goes out
  // of scope rather than until its last use. The 'do' ends it before 'x' is
  // read again below.
  do {
    let r: Swift.Ref<Swift.Cell<CInt>> = pick(&x)
    r.value.value = 3
  }
  let c: Swift.Ref<Swift.ConstCell<CInt>> = constPick(x)
  _ = c.value.value
}

func takesPointer(_ p: UnsafeMutablePointer<CInt>) {}
func takesConstPointer(_ p: UnsafePointer<CInt>) {}

// The safe overloads are disfavored: code that does not ask for a 'Ref' keeps
// calling the original functions.
func unannotated(_ x: inout CInt) {
  let p = unsafe pick(&x)
  let _: UnsafeMutablePointer<CInt> = unsafe p
  let q = unsafe constPick(x)
  let _: UnsafePointer<CInt> = unsafe q
  unsafe pick(&x).pointee = 4
  _ = unsafe constPick(x).pointee
  unsafe takesPointer(pick(&x))
  unsafe takesConstPointer(constPick(x))
}
