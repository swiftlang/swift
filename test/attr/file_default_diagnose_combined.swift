// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// TODO: use #if once that works correctly for file default diagnostic

// RUN: cat %t/defaults.swift %t/body.swift > %t/top.swift
// RUN: cat %t/body.swift %t/defaults.swift > %t/bottom.swift

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -strict-memory-safety -typecheck -verify %t/top.swift
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -strict-memory-safety -typecheck -verify %t/bottom.swift

// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- defaults.swift
using @diagnose(DeprecatedDeclaration, as: ignored)
using @diagnose(StrictMemorySafety, as: error, reason: "I love strict memory safety! <3")

//--- body.swift
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
