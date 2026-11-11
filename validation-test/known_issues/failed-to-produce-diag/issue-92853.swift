// rdar://189011005 (https://github.com/swiftlang/swift/issues/92853)
// RUN: %target-typecheck-verify-swift

struct X {
  func make<T>() -> T {
    fatalError()
  }

  func make<T>(_ type: T.Type) -> T {
    fatalError()
  }
}

let x = X().make() // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
