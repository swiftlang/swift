// rdar://177995638
// RUN: %target-typecheck-verify-swift

func foo<each I: Any>(bar: repeat each I) {
  repeat "\(each bar as each I)"
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
