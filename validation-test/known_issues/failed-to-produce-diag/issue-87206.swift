// rdar://170281977 (https://github.com/swiftlang/swift/issues/87206)
// RUN: %target-typecheck-verify-swift

func foo<each T>(_ e: repeat each T) {
  repeat ({ // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    defer {
      _ = each e
    }
  })
}
