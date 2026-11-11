// rdar://188271308
// RUN: %target-typecheck-verify-swift

final class R {
  static func get() -> R! { nil }
}

func test(_ o: R) {
  _ = o.missing(R.get()!) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
