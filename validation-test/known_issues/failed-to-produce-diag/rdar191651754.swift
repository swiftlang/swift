// rdar://191651754
// RUN: %target-typecheck-verify-swift

func keys() -> [String]? { nil }

func f() {
  var d: [String: Int] = [:]
  (keys ?? []).forEach { key in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    _ = 0
    d[key] = 1
  }
}
