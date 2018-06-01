// RUN: %target-swift-frontend -emit-sil %s

protocol X1 {
  associatedtype X3 : X4
}

protocol X4 {
  associatedtype X15
}

protocol X7 { }

protocol X9 : X7 {
  associatedtype X10 : X7
}

struct X12 : X9 {
  typealias X10 = X12
}

struct X13<I1 : X7> : X9 {
  typealias X10 = X13<I1>
}

struct X14<G : X4> : X4 where G.X15 : X9 {
  typealias X15 = X13<G.X15.X10>
}

struct X17<A : X4> : X1 where A.X15 == X12 {
  typealias X3 = X14<A>
}

struct X18 : X4 {
  typealias X15 = X12
}

@_transparent
func callee<T>(_: T) where T : X1 {
  let _: T.X3.X15? = nil
}

func caller(b: X17<X18>) {
  callee(b)
}
