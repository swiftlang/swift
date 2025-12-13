// RUN: %target-swift-frontend -emit-ir %s

struct P<A, B> { let l: A; let r: B }
struct Value { let v: Double? }

func +<A, B, C>(l: P<A, C>, r: P<B, C>) -> P<(A, B), C> {
  return P(l: (l.l, r.l), r: l.r)
}

infix operator <!>

func <!> <A>(l: A?, r: A) -> A {
  return l == nil ? r : l!
}

func <!> <A, B, C>(l: P<A, B>, r: () -> C) -> P<A, C> {
  return P(l: l.l, r: r())
}

let xs: [Value] = []
let a: String = String(xs.reduce(0.0, { (acc, i) in 
  acc + (i.v <!> 0) 
}))

print(a)
