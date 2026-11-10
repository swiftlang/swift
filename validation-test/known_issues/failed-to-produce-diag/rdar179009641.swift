// rdar://179009641
// RUN: %target-typecheck-verify-swift

struct S {
  let value: UInt
}

struct Foo {
  init?(rawValue: Int) {}
}

func takesFoo(_: Foo?) {}

func test(s: S?) {
  takesFoo(Foo(rawValue: s?.value) ?? nil) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
