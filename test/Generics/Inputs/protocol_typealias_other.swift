public protocol P1 {
  typealias X = Y.Z
  associatedtype Y : P2
}

public protocol P2 {
  associatedtype Z
  typealias C = Array<Z>
}
