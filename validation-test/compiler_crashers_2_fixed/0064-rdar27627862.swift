// RUN: %target-typecheck-verify-swift

enum Term<S> where S: Sequence, S.Iterator.Element == Term {
// expected-error@-1 *{{generic enum 'Term' has self-referential generic requirements}}
    case Cons(head: String, tail: S)
}

func produce<S>(s: S) -> Term<S> {
  return .Cons(head: "hi", tail: s)
}
