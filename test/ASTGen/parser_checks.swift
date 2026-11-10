// Checks the C++ parser makes on declarations and statements, checked against
// both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// Duplicate attributes and modifiers.
@inline(never) @inline(never) func f1() {}
// expected-error@-1 {{duplicate attribute}}
// expected-note@-2 {{attribute already specified here}}

public public func f2() {}
// expected-error@-1 {{duplicate modifier}}
// expected-note@-2 {{modifier already specified here}}

open private class C1 {}
// expected-error@-1 {{multiple incompatible access-level modifiers specified}}
// expected-note@-2 {{previous modifier specified here}}

// A modifier spelled as an attribute.
@final class C2 {}
// expected-error@-1 {{'final' is a declaration modifier, not an attribute}} {{1-2=}}

// 'return' followed by an expression at the same indentation.
func f3() -> Int {
  return
  5 // expected-warning {{expression following 'return' is treated as an argument of the 'return'}}
  // expected-note@-1 {{indent the expression to silence this warning}}
}

// Computed properties need a type.
struct S {
  var computed { 0 }
  // expected-error@-1 {{computed property must have an explicit type}} {{15-15=: <# Type #>}}
  // expected-error@-2 {{type annotation missing in pattern}}
}

// Switch cases need a body, and nothing may follow 'default'.
func f4(_ x: Int) {
  switch x {
  case 1: // expected-error {{'case' label in a 'switch' must have at least one executable statement}} {{10-10= break}}
  default: break
  case 2: break // expected-error {{additional 'case' blocks cannot appear after the 'default' block of a 'switch'}}
  }
}

// Protocol initializers have no body.
protocol P {
  init() {} // expected-error {{protocol initializers must not have bodies}}
}
