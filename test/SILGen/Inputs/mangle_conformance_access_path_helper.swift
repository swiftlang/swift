public protocol P {
  associatedtype T
}

public protocol Q {
  associatedtype U
}

public protocol R {
  associatedtype U : Q where U.U : P
}

public struct G<T> {}
