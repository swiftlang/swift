// RUN: %target-swift-emit-silgen %s

func f0<V>(_: () -> any P<V>) {}
func f1<V>(_: V.Type, _: () -> any P<V>) {}
func f2<V>(_: (V) -> (), _: () -> any P<V>) {}

protocol P<A> {
  associatedtype A
}

struct G<A>: P {}

func g1() -> some P<Int> {
  return G<Int>()
}

class B {}
class C: B {}
class D: C, P {
  typealias A = Bool
}

func g2() -> some C & P {
  return D()
}

func g3() -> (some P<Bool>, some C & P) {
  return (G<Bool>(), D())
}

func test() {
  let _: () -> any P<Int> = g1
  let _: () -> B = g2
  let _: () -> (any P, B) = g3

  f0(g1)
  f1(Int.self, g1)
  f2({ $0 as Int }, g1)

  f0({ g1() })
  f1(Int.self, { g1() })
  f2({ $0 as Int }, { g1() })
}

