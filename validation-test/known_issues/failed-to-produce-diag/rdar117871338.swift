// rdar://117871338
// RUN: %target-typecheck-verify-swift

struct S {}

func g<T>(_ fn: (T) -> Void) {}

func test1(s: S) {
  g { x in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    switch s.value? {
    case 1: break
    }
  }
}

func test2(s: S) {
  switch s.value? { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  case 1: break
  }
}
