// RUN: %target-swift-frontend %s -emit-ir
struct X<T: Q> {
  func f(_: T.Z) { }
}

protocol P {
  associatedtype A
  associatedtype B
}

protocol Q {
  associatedtype C: P
  typealias Z = (C.A, C.B)
}
