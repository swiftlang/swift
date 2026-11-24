// rdar://74558153
// RUN: %target-typecheck-verify-swift

func test(closure: Int, body: (Int) throws -> String) rethrows -> String {
  try withoutActuallyEscaping(closure, do: body) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
