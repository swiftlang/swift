// rdar://156144309
// RUN: %target-typecheck-verify-swift

func f1() { let _ = (.cat.dog()) & (.cat.dog()) } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
func f2() { let _ = (.cat.dog()) | (.cat.dog()) } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
func f3() { let _ = (.cat.dog()) ^ (.cat.dog()) } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
