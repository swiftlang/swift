// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/87552

struct S {
  init(_ xs: some Sequence<Int>) {}
}
func foo<T>(_ fn: T?) -> [T] { [] }

let _ = S(foo(.init()))
let _ = S(foo({ .init() }()))

let _ = S(foo(if .random() { .init() } else { .init() })) // expected-error {{'if' may only be used as expression}}

let _ = S.init(foo(.init()))
let _ = S.init(foo({ .init() }()))
