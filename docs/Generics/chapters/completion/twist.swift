protocol S {
  associatedtype A
  associatedtype B
  associatedtype C: S where Self == Self.C.C, Self.B == Self.C.A
}

extension S {
  func f(x: Self.C.B) -> Self.A {
    return x
  }
}
