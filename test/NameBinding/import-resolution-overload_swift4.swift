// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/overload_vars.swift
// RUN: %target-typecheck-verify-swift -swift-version 4 -I %t

import overload_vars

func useString(_ str: String) {}

// Ensure we maintain compatibility with Swift 4's overload signature rules.
// Variables defined in extensions of generic types had different overload
// signatures to other variables, so allow overloading in such cases (SR-7341).
extension HasFooGeneric {
  var foo: String { return "" } // `foo` isn't defined in a generic extension in the other module, so allow overloading in Swift 4 mode.
  var bar: String { return "" } // `bar` is defined in a generic extension in the other module, so `bar: String` always shadows it.

  func baz() {
    let x1: Int = foo // Make sure `foo: Int` is in the overload set.
    _ = x1

    let x2: String = foo // Make sure `foo: String` is in the overload set.
    _ = x2

    let y1 = bar // No ambiguity error.
    useString(y1) // Make sure we resolved to `bar: String`.

    // Make sure `bar: Int` is not in the overload set.
    let y2: Int = bar // expected-error {{cannot convert}}
    _ = y2
  }
}

// But for non-generic types, the variable overload signature was always the
// null type, so `foo/bar: String` shadows `foo/bar: Int`.
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
