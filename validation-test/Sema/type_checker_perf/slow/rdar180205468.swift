// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

enum E { case x, y }

struct S {
    var a: E = .x
    var b: E? = nil
    var c: E = .x
}

func f<A, B>(
    s: B,
    k: WritableKeyPath<B, A>,
    fn: @escaping (B) -> Bool
) {}

func slow() {
  let s = S()
  let _ = f(  // expected-error {{reasonable time}}
      s: s,
      k: \.a,
      fn: {
          $0.a == .y &&
          $0.b == .y &&
          $0.c == .y
      }
  )
}
