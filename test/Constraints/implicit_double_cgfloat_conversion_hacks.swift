// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-swift-frontend -swift-version 5 -typecheck %s -solver-enable-performance-hacks
// RUN: %target-swift-emit-silgen -swift-version 5 -solver-enable-performance-hacks %s

// REQUIRES: objc_interop
import Foundation

// Move this back to implicit_double_cgfloat_conversion.swift when fixed.

func test_explicit_cgfloat_use_avoids_ambiguity(v: Int) {
  func test(_: CGFloat) -> CGFloat { 0 } // expected-note {{found this candidate}}
  func test(_: Double) -> Double { 0 } // expected-note {{found this candidate}}

  func hasCGFloatElement<C: Collection>(_: C) where C.Element == CGFloat {}

  let arr = [test(CGFloat(v))]
  hasCGFloatElement(arr) // Ok

  var total = 0.0 // This is Double by default
  total += test(CGFloat(v)) + CGFloat(v)
  // expected-error@-1 {{ambiguous use of 'test'}}
}