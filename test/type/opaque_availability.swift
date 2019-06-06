// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.9 -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-macosx10.15 -typecheck %s
// REQUIRES: OS=macosx

protocol P {}
struct X: P {}

func alwaysOpaque() -> some P { return X() } // expected-error{{'some' return types are only available}} expected-note{{add @available}}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
func sometimesOpaque() -> some P { return X() }


