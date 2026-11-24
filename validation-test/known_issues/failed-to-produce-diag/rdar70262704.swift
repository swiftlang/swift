// rdar://70262704
// RUN: %target-typecheck-verify-swift

_ = (Int, Int)() // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
