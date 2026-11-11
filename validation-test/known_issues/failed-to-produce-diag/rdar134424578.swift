// rdar://134424578
// RUN: %target-typecheck-verify-swift

func f() {
  for (a, b, c) in [ // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    ("a0", "b0", nil),
    ("a1", "b1", nil),
    ("a2", "b2", nil),
    ("a3", "b3", nil),
    ("a4", "b4", nil),
    ("a5", "b5", nil),
    ("a6", "b6", nil),
    ("a7", "b7", nil),
    ("a8", "b8", nil),
    ("a9", "b9", nil),
  ] {
    print(a, b, c ?? "nil")
  }
}
