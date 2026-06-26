// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -strict-memory-safety -typecheck -verify %s

// REQUIRES: swift_feature_DefaultIsolationPerFile

using @diagnose(DeprecatedDeclaration, as: ignored)
using @diagnose(StrictMemorySafety, as: error, reason: "I love strict memory safety! <3")

@available(*, deprecated)
func deprecatedThing() {}

@unsafe
func unsafeThing() {}

func caller() {
  deprecatedThing()
  unsafeThing() // expected-error {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'unsafeThing()'}}
}

@diagnose(DeprecatedDeclaration, as: error)
func callerWithPartialOverride() {
  deprecatedThing() // expected-error {{'deprecatedThing()' is deprecated}}
  unsafeThing() // expected-error {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'unsafeThing()'}}
}

@diagnose(DeprecatedDeclaration, as: warning)
class SplitOverride {
  func methodA() {
    deprecatedThing() // expected-warning {{'deprecatedThing()' is deprecated}}
    unsafeThing() // expected-error {{expression uses unsafe constructs but is not marked with 'unsafe'}}
    // expected-note@-1 {{reference to unsafe global function 'unsafeThing()'}}
  }

  @diagnose(StrictMemorySafety, as: warning)
  func methodB() {
    deprecatedThing() // expected-warning {{'deprecatedThing()' is deprecated}}
    unsafeThing() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
    // expected-note@-1 {{reference to unsafe global function 'unsafeThing()'}}
  }
}
