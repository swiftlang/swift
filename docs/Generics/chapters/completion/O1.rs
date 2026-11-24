trait N {
  type A: N;
}

trait O: N<A: O> {}  // error: cycle detected
