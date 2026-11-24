// rdar://185292204 (https://github.com/swiftlang/swift/issues/91587)
// RUN: %target-typecheck-verify-swift

func f(c: Character, s: String) {
  if c != s.index(s.startIndex, offsetBy: 4) {} // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
