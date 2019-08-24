// RUN: %target-swift-frontend %s -emit-ir

enum Term<S> where S: Sequence, S.Iterator.Element == Term {
    case Cons(head: String, tail: S)
}

func produce<S>(s: S) -> Term<S> {
  return .Cons(head: "hi", tail: s)
}
