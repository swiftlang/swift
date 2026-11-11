// rdar://179620445 (https://github.com/swiftlang/swift/issues/89940)
// RUN: %target-typecheck-verify-swift

struct S<T> where T: Collection {
  static func f() {
    _ = Self<String>() // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
