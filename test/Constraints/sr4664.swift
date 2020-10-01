// RUN: %target-typecheck-verify-swift

struct M<T> where T : Collection { // expected-note {{where 'T' = 'X.Y'}}
  static func f(a: T, b: T) -> [E<T.Iterator.Element>] {
  }
}

enum E<T> {}

struct S {}

struct X {
  struct Y {
    let s: [S]
  }

  let y: [Y]
}

let x = X(y: [])
let a = M.f(a: x.y[0], b: x.y[1])
// expected-error@-1 {{generic struct 'M' requires that 'X.Y' conform to 'Collection'}}
