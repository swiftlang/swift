// RUN: %target-swift-frontend %s -emit-ir

protocol P {
  associatedtype A
  associatedtype B
}

protocol Q : P {
  associatedtype M
  typealias A = M

}


extension Q {
  typealias B = M
}

protocol R {
  associatedtype S

  init()
}

extension R {
  init<V : Q>(_: V) where V.M == Self {
    let _ = V.A.self
    let _ = V.B.self
    let _ = V.M.self
    let _ = Self.self

#if false
    let _: V.M.Type = V.A.self
    let _: V.M.Type = V.B.self
    let _: V.M.Type = Self.self

    let _: V.A.Type = V.M.self
    let _: V.A.Type = V.B.self
    let _: V.A.Type = Self.self

    let _: V.B.Type = V.M.self
    let _: V.B.Type = V.A.self
    let _: V.B.Type = Self.self

    let _: Self.Type = V.A.self
    let _: Self.Type = V.B.self
    let _: Self.Type = V.M.self
#endif

    self.init()
  }
}
