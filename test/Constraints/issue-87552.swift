// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/87552

struct S {
  init(_ xs: some Sequence<Int>) {}
}
func foo<T>(_ fn: T?) -> [T] { [] }

// FIXME: This ought to work
let _ = S(foo(.init())) // expected-error {{member 'init()' in 'Int?' produces result of type 'Int', but context expects 'Int?'}}
let _ = S(foo({ .init() }()))

let _ = S(foo(if .random() { .init() } else { .init() })) // expected-error {{'if' may only be used as expression}}

let _ = S.init(foo(.init()))
let _ = S.init(foo({ .init() }()))
