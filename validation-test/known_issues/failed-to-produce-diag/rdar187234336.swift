// rdar://187234336
// RUN: %target-typecheck-verify-swift

func load<T>() -> T { fatalError() }
func load<R>(arg: Int) -> R { fatalError() }

func g() {
  load(arg: 0)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
