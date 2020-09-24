// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T : Q
}

protocol Q {
  associatedtype T : R

  var t: T { get }
}

protocol R {}

func takesR<T : R>(_: T) {}

class C<T : Q> : P {}

struct Outer<T : P> {
  struct Inner<U> where T : C<U> {
    func doStuff(_ u: U) {
      takesR(u.t)
    }
  }
}
