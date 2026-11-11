// rdar://184059895
// RUN: %target-typecheck-verify-swift

func getPath<T>(index: Int) -> KeyPath<[T], T> {
  return \[Int].[index] // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
