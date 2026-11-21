protocol Z4 {
  associatedtype A: Z4 where Self == Self.A.A.A.A
}
