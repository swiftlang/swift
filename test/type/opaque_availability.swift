// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.9 -typecheck %s -verify -verify-additional-prefix only-available-
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.15 -typecheck %s -verify
// REQUIRES: OS=macosx

protocol P {}
struct X: P {}

func alwaysOpaque() -> some P { return X() } // expected-only-available-error{{'some' return types are only available}} expected-only-available-note{{add @available}}

@available(SwiftStdlib 5.1, *)
func sometimesOpaque() -> some P { return X() }

struct NotP {}

@available(*, unavailable)
extension NotP: P {} // expected-note {{conformance of 'NotP' to 'P' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
func requireP() -> some P {
  NotP() // expected-error {{conformance of 'NotP' to 'P' is unavailable}}
}
