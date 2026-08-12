// RUN: %target-typecheck-verify-swift -swift-version 5
//
// `DiagnosticEngine::isDiagnosticGroupEnabled` skips the SwiftWarningControl
// region tree, and the corresponding SwiftParser parse, unless the parser
// recorded a `@diagnose` in this file. Members and function bodies are parsed
// lazily, so check that a `@diagnose` in one of those still takes effect even
// though an earlier declaration in the file is checked first.

@available(*, deprecated)
func dep() -> Bool { return false }

// Checked before anything below has been parsed, and must stay unaffected by
// the `@diagnose` attributes that appear later in the file.
func earlyUnaffected() -> Bool {
  return dep() // expected-warning {{'dep()' is deprecated}}
}

// `@diagnose` on a member of a type: the member list is parsed lazily.
struct Outer {
  @diagnose(DeprecatedDeclaration, as: error)
  func lateMember() -> Bool {
    return dep() // expected-error {{'dep()' is deprecated}}
  }

  @diagnose(DeprecatedDeclaration, as: ignored)
  func lateMemberIgnored() -> Bool {
    return dep()
  }
}

// `@diagnose` on a local declaration inside a function body: bodies are parsed
// lazily.
func host() {
  @diagnose(DeprecatedDeclaration, as: error)
  func lateLocal() -> Bool {
    return dep() // expected-error {{'dep()' is deprecated}}
  }
  _ = lateLocal()
}

// A nested type inside a lazily parsed member list.
enum DeepOuter {
  struct Inner {
    @diagnose(DeprecatedDeclaration, as: error)
    var deep: Bool { dep() } // expected-error {{'dep()' is deprecated}}
  }
}

// Trivia is permitted between `@` and the attribute name
@ diagnose(DeprecatedDeclaration, as: error) // expected-warning {{extraneous whitespace between '@' and attribute name}}
func spacedAttribute() -> Bool {
  return dep() // expected-error {{'dep()' is deprecated}}
}
