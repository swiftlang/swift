// Test that overrides in other source files get checked lazily.

// Make sure the overrides are resolved... but we don't diagnose a missing
// 'override' keyword from another source file.
// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/overrideB.swift %S/Inputs/overrideC.swift -verify

// Make sure we still diagnose the missing 'override' when looking at the
// source file where it occurs.
// RUN: not %target-swift-frontend -typecheck %s %S/Inputs/overrideB.swift -primary-file %S/Inputs/overrideC.swift 2> %t.err
// RUN: %FileCheck %s < %t.err

// expected-no-diagnostics

class Foo {
  func foo(sub: Sub) {
    sub.bar(Into<Int>())
  }
}

struct Into<T> { }

// CHECK: overriding declaration requires an 'override' keyword
