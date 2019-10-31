// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/overload_vars.swift
// RUN: %target-typecheck-verify-swift -swift-version 5 -I %t

import overload_vars

func useString(_ str: String) {}

// In Swift 5, properties from this module currently always shadow properties
// from the other module – therefore meaning that the properties from the other
// module never show up in the overload set.
// FIX-ME: It seems reasonable for both to show up in the overload set.

extension HasFooGeneric {
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
