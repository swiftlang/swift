// rdar://76951258
// RUN: %target-typecheck-verify-swift

func test<T>(a: () -> T, b: () -> T) {}

struct A {}
struct B {}

test { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  return A()
} b: {
  return B()
}
