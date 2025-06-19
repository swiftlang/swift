// RUN: %target-swift-emit-silgen %s

protocol N {
  associatedtype A: N
}

struct G<T>: N {
  typealias A = G<G<T>>
}

protocol P<A> {
  associatedtype A
}

struct GG<T>: P {
  typealias A = T
}

func f<T: N>(_: T) -> some P<T.A.A> {
  return GG<T.A.A>()
}

func g<T: P>(_: T) -> T.A { fatalError() }

func h(_: G<G<G<Int>>>) {}

h(g(f(G<Int>())))
