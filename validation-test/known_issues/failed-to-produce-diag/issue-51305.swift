// rdar://44616337 (https://github.com/swiftlang/swift/issues/51305)
// RUN: %target-typecheck-verify-swift

typealias Executable<Input> = (Input) -> ()
let test: Executable<Void>? = { }
// expected-error@-1 {{closure tuple parameter 'Void' does not support destructuring}}
let a: ((Int) -> ())? = { }
// expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
