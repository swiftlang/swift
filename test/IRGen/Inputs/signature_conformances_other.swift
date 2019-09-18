protocol Q {
  associatedtype T
}

struct AlsoConforms<T> : Q {}

func takesQ<T : Q>(_: T) {}

protocol P : Q {
  associatedtype U
}

struct Conforms<T, U> : P {}

func takesP<T : P>(_: T) {}