protocol P1 {
  associatedtype A
}

protocol P2 {
  associatedtype A
}

struct G<T: P1 & P2> {}
