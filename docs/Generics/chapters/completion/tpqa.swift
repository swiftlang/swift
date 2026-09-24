protocol P {
  associatedtype A: P
}

protocol Q {
  associatedtype A: Q
}

struct G<T: P & Q> {}
