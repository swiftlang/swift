// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -verify %s

// REQUIRES: swift_feature_DefaultIsolationPerFile

using @diagnose(DeprecatedDeclaration, as: error)

@available(*, deprecated)
func deprecatedThing() {}

func callerA() {
  deprecatedThing() // expected-error {{'deprecatedThing()' is deprecated}}
}

func callerB() {
  deprecatedThing() // expected-error {{'deprecatedThing()' is deprecated}}
}

class UseDefault {
  func methodA() {
    deprecatedThing() // expected-error {{'deprecatedThing()' is deprecated}}
  }

  @diagnose(DeprecatedDeclaration, as: warning)
  func methodB() {
    deprecatedThing() // expected-warning {{'deprecatedThing()' is deprecated}}
  }

  @diagnose(DeprecatedDeclaration, as: ignored)
  func methodC() {
    deprecatedThing()
  }
}

@diagnose(DeprecatedDeclaration, as: ignored)
class Override {
  func method() {
    deprecatedThing()
  }
}
