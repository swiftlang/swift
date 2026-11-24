// rdar://166971606 (https://github.com/swiftlang/swift/issues/73199)
// RUN: %target-typecheck-verify-swift

let _: (@MainActor @Sendable () -> Void)? = true ? { } : nil // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
let _: (@Sendable () -> Void)? = true ? { } : nil // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
let _: (@MainActor () -> Void)? = true ? { } : nil // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
