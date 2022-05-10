// RUN: %target-swift-frontend -emit-ir %s

protocol P {
  associatedtype T : Q

  func makeT() -> T
}

protocol Q {
  associatedtype U : R

  func makeU() -> U
}

protocol R {
  init()
}

class C<U : R> : Q {
  func makeU() -> U {
    return U()
  }
}

func takesR<T : R>(_: T.Type) {}

func takesPWithC<T : P, U>(_ t: T) -> U where T.T : C<U> {
  takesR(T.T.U.self)

  let c: C<U> = t.makeT()
  let u: U = c.makeU()

  return u
}

