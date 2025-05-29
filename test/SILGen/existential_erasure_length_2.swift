// RUN: %target-swift-emit-silgen %s

protocol N {
  associatedtype A: N
}

protocol P<A> {
  associatedtype A: N
  associatedtype B

  func f0(_: A) -> B
  func f1(_: A.A) -> B
  func f2(_: A.A.A) -> B
}

struct G<T>: N {
  typealias A = G<G<T>>
}

func call(x: any P<G<Int>>) -> (Any, Any, Any) {
  let y0 = x.f0(G<Int>())
  let y1 = x.f1(G<G<Int>>())
  let y2 = x.f2(G<G<G<Int>>>())
  return (y0, y1, y2)
}
