// rdar://167101115
// RUN: %target-typecheck-verify-swift

func foo(_ fn: () -> String) {}
func foo(_ fn: () -> Int) {}

foo {} // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
