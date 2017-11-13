// RUN: %target-typecheck-verify-swift -swift-version 4.1

class NotEquatable {}

func test_ArrayOfNotEquatableIsNotEquatable() {
  var a = [ NotEquatable(), NotEquatable() ]
  // FIXME: This is an awful error.
  if a == a {} // expected-error {{binary operator '==' cannot be applied to two '[NotEquatable]' operands}}
  // expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: }}
}

func test_SE0132Deprecations() {
  var a = [1, 2, 3]
  
  _ = a.index(of: 2)  // expected-warning {{'index(of:)' is deprecated: renamed to 'firstIndex(of:)'}}
  // expected-note @-1 {{use 'firstIndex(of:)' instead}} {{9-14=firstIndex}}
  
  _ = a.index(where: { $0 == 2})  // expected-warning {{'index(where:)' is deprecated: renamed to 'firstIndex(where:)'}}
  // expected-note @-1 {{use 'firstIndex(where:)' instead}} {{9-14=firstIndex}}
  
  _ = a.index { $0 == 2 }  // expected-warning {{'index(where:)' is deprecated: renamed to 'firstIndex(where:)'}}
  // expected-note @-1 {{use 'firstIndex(where:)' instead}} {{9-14=firstIndex}}
  
  _ = a.starts(with: [1, 2]) // expected-warning {{'starts(with:)' is deprecated: renamed to 'hasPrefix(_:)'}}
  // expected-note @-1 {{use 'hasPrefix(_:)' instead}} {{9-15=hasPrefix}} {{16-22=}}
  
  _ = a.starts(with: [1, 2], by: ==) // expected-warning {{'starts(with:by:)' is deprecated: renamed to 'hasPrefix(_:by:)'}}
  // expected-note @-1 {{use 'hasPrefix(_:by:)' instead}} {{9-15=hasPrefix}} {{16-22=}}
  
  _ = a.removeFirst(1) // expected-warning {{'removeFirst' is deprecated: renamed to 'removePrefix(_:)'}}
  // expected-note @-1 {{use 'removePrefix(_:)' instead}} {{9-20=removePrefix}}
  
  _ = a.removeLast(1) // expected-warning {{'removeLast' is deprecated: renamed to 'removeSuffix(_:)'}}
  // expected-note @-1 {{use 'removeSuffix(_:)' instead}} {{9-19=removeSuffix}}
  
  _ = a.dropFirst(1) // expected-warning {{'dropFirst' is deprecated: renamed to 'removingPrefix(_:)'}}
  // expected-note @-1 {{use 'removingPrefix(_:)' instead}} {{9-18=removingPrefix}}
  
  _ = a.dropLast(1) // expected-warning {{'dropLast' is deprecated: renamed to 'removingSuffix(_:)'}}
  // expected-note @-1 {{use 'removingSuffix(_:)' instead}} {{9-17=removingSuffix}}
  
  _ = a.dropFirst() // expected-warning {{'dropFirst()' is deprecated: renamed to 'removingFirst()'}}
  // expected-note @-1 {{use 'removingFirst()' instead}} {{9-18=removingFirst}}
  
  _ = a.dropLast() // expected-warning {{'dropLast()' is deprecated: renamed to 'removingLast()'}}
  // expected-note @-1 {{use 'removingLast()' instead}} {{9-17=removingLast}}
}
