trait N {
  type A: N;
}

trait O: N<A = Self::OA> {
  type OA: O;
}
