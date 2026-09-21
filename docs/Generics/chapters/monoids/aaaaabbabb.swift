protocol M {
  associatedtype A: M
  associatedtype B: M
    where A.A.A == A, A.B.B.A == B.B
}
