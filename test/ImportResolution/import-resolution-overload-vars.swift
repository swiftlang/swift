// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/overload_vars.swift
// RUN: %target-typecheck-verify-swift -I %t -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -I %t -language-mode 4 -verify-additional-prefix swift4- -verify-additional-file overload_vars.HasFooGeneric.foo

import overload_vars

func useString(_ str: String) {}

// In Swift 5, properties from this module currently always shadow properties
// from the other module – therefore meaning that the properties from the other
// module never show up in the overload set.
// FIX-ME: It seems reasonable for both to show up in the overload set.

// https://github.com/apple/swift/issues/49889
// Ensure we maintain compatibility with Swift 4's overload signature rules.
// Variables defined in extensions of generic types had different overload
// signatures to other variables, so allow overloading in such cases.

extension HasFooGeneric {
  var foo: String { return "" }
  // expected-swift4-note@-1 {{found this candidate}}
  var bar: String { return "" }

  func baz() {
    let x1 = foo // No ambiguity error in swift 5 mode onward.
    // expected-swift4-error@-1 {{ambiguous use of 'foo'}}
    // expected-swift4-note@overload_vars.HasFooGeneric.foo:2 {{found this candidate in module 'overload_vars'}}
    useString(x1) // Make sure we resolved to `foo: String`.

    // Make sure `foo: Int` is not in the overload set in swift 5 mode onward.
    // expected-swift5-error@+1 {{cannot convert}}
    let x2: Int = foo
    _ = x2

    let a1: String = foo // Make sure `foo: String` is in the overload set.
    _ = a1

    let y1 = bar // No ambiguity error.
    useString(y1) // Make sure we resolved to `bar: String`.

    // Make sure `bar: Int` is not in the overload set.
    let y2: Int = bar // expected-error {{cannot convert}}
    _ = y2
  }
}

extension HasFooNonGeneric {
  var foo: String { return "" }
  var bar: String { return "" }

  func baz() {
    let x1 = foo // No ambiguity error.
    useString(x1) // Make sure we resolved to `foo: String`.

    // Make sure `foo: Int` is not in the overload set.
    let x2: Int = foo // expected-error {{cannot convert}}
    _ = x2

    let y1 = bar // No ambiguity error.
    useString(y1) // Make sure we resolved to `bar: String`.

    // Make sure `bar: Int` is not in the overload set.
    let y2: Int = bar // expected-error {{cannot convert}}
    _ = y2
  }
}
