// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -primary-file %t/Client.swift -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

// Symbols referenced only from the body of an inline function in a header
// should be weakly linked if their module is imported @_weakLinked.

//--- module.modulemap

module Strong {
  header "Strong.h"
}

module Weak {
  module Sub {
    header "Weak.h"
  }
}

//--- Strong.h

extern int strong_fn(void);
extern int redeclared_fn(void);

//--- Weak.h

#include "Strong.h"

extern int weak_fn(void);
extern int weak_var;
extern int redeclared_fn(void);

static inline int weak_inline_fn(void) {
  return weak_fn() + weak_var + strong_fn() + redeclared_fn();
}

//--- Client.swift

import Strong
@_weakLinked import Weak

// CHECK-DAG: declare extern_weak i32 @weak_fn()
// CHECK-DAG: @weak_var = extern_weak global i32
// CHECK-DAG: declare i32 @strong_fn()
// CHECK-DAG: declare extern_weak i32 @redeclared_fn()

public func test() -> Int32 {
  return redeclared_fn() + weak_inline_fn()
}
