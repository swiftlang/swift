// rdar://185011042 (https://github.com/swiftlang/swift/issues/78417)
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct S<each T> {
  var x: (repeat some Collection<each T>) = () // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
