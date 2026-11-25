// rdar://106097842
// RUN: %target-typecheck-verify-swift

struct S: ~Copyable {
  consuming func f() {
    let _: Int = { () -> Int in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      discard self
      return 0
    }()
  }
}
