// rdar://179503417 (https://github.com/swiftlang/swift/issues/89918)
// RUN: %target-typecheck-verify-swift

let x = type(of: Int.foo) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
