// RUN: %target-typecheck-verify-swift -solver-scope-threshold=40000

// This was relatively fast in 6.3 and then regressed recently.

struct S {
  static var x = S()
}

protocol P {}

struct G<T: P> {
  typealias A = (T.Type) -> S

  static var x: A { fatalError() }

  init() {}
}

func &&(_: S, _: S) -> S {}

func &&<T: P>(_: @escaping G<T>.A, _: @escaping G<T>.A) -> G<T>.A {
  fatalError()
}

func &&<T: P>(_: @escaping G<T>.A, _: S) -> S {
  fatalError()
}

func &&<T: P>(_: S, _: @escaping G<T>.A) -> S {
  fatalError()
}

struct S2 {
  var x: Int

  static func ==(lhs: S2, rhs: S2) {
    // expected-error@+1 {{reasonable time}}
    return lhs.x == rhs.x && lhs.x == rhs.x && lhs.x == rhs.x &&
           lhs.x == rhs.x && lhs.x == rhs.x && lhs.x == rhs.x &&
           lhs.x == rhs.x && lhs.x == rhs.x && lhs.x == rhs.x &&
           lhs.x == rhs.x && lhs.x == rhs.x && lhs.x == rhs.x
  }
}

