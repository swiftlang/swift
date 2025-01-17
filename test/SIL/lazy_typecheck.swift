// RUN: %target-swift-frontend -emit-sil %s -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck -verify

// SIL diagnostics should not crash on invalid types.
final class C {
  private let x: Nonexistent // expected-error {{cannot find type 'Nonexistent' in scope}}

  init(x: Nonexistent) { // expected-error {{cannot find type 'Nonexistent' in scope}}
    self.x = x
  }
}
