// RUN: %target-typecheck-verify-swift -swift-version 4.1

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  // FIXME: This is an awful error.
  if a == a {} // expected-error {{binary operator '==' cannot be applied to two '[NotEquatable]' operands}}
  // expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: }}
}

func test_SE0132Deprecations() {
  let a = [1, 2, 3]
  
  _ = a.index(of: 2)  // expected-warning {{'index(of:)' is deprecated: renamed to 'firstIndex(of:)'}}
  // expected-note @-1 {{use 'firstIndex(of:)' instead}} {{9-14=firstIndex}}
  
  _ = a.index(where: { $0 == 2})  // expected-warning {{'index(where:)' is deprecated: renamed to 'firstIndex(where:)'}}
  // expected-note @-1 {{use 'firstIndex(where:)' instead}} {{9-14=firstIndex}}
  
  _ = a.index { $0 == 2 }  // expected-warning {{'index(where:)' is deprecated: renamed to 'firstIndex(where:)'}}
  // expected-note @-1 {{use 'firstIndex(where:)' instead}} {{9-14=firstIndex}}
}
