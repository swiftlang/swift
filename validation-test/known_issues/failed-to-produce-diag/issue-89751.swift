// rdar://178939955 (https://github.com/swiftlang/swift/issues/89751)
// RUN: %target-typecheck-verify-swift

final class A {
  var x: Int?
}

func test1(a: A?) {
  a?.x = a?.x = 1 // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

struct S {
  var x: Int?
}

func test2(s: inout S?) {
  s?.x = s?.x = 1 // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

final class C {}

final class D {
  var x: C?
}

final class B {
  var d: D?
}

func test3(b: B, c: C) {
  b.d?.x = b.d?.x = c // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func test4(b: B, c: C) {
  b.d?.x = (b.d?.x = c) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func correct(a: A?) {
  a?.x = a?.x
}
