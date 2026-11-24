// rdar://192839924 (https://github.com/swiftlang/swift/issues/94347)
// RUN: %target-typecheck-verify-swift

func throwingG<R>(_ body: () throws -> R) throws -> R {
    try body()
}

func throwingF(_ x: Int) -> Bool {
    try! throwingG { x } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
