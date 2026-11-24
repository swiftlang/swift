// rdar://167912818 (https://github.com/swiftlang/swift/issues/86454)
// RUN: %target-typecheck-verify-swift -swift-version 6 -target %target-swift-5.9-abi-triple

protocol P<Value>: Sendable {
  associatedtype Value: Sendable
}

struct S<each T: Sendable> {
  typealias F = @Sendable ((repeat each T)) -> [(repeat each T)]

  let f: F?

  init<each U: P>(_ u: (repeat each U)?) where (repeat (each U).Value) == (repeat each T) {
    self.f = u.map { u in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      { _ in
        let _ = Self.g(u)
        return []
      }
    }
  }

  static func g<each U: P>(_: (repeat each U)) -> Int { 0 }
}
