// rdar://192469248 (https://github.com/swiftlang/swift/issues/94204)
// RUN: %target-typecheck-verify-swift

struct S: _ExpressibleByImageLiteral {
  init(imageLiteralResourceName path: String) {}
}

let _: S = #imageLiteral(resourceName: String.x) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
