protocol Q {
  associatedtype T
}

protocol P : Q {
  associatedtype U
}

struct Conforms<T, U> : P {}

func takesP<T : P>(_: T) {}