// rdar://177424839 (https://github.com/swiftlang/swift/issues/89251)
// RUN: %target-typecheck-verify-swift

func f(_: String) -> [UInt8] {
  fatalError()
}

func g(s: String) {
  let _: [UInt8] = s.map(f) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
