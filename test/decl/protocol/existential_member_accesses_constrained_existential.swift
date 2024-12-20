// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple

/// Used to verify the type of an expression. Use like this:
/// ```
/// var types = SwiftTypePair(typeOf: expr, type2: SwiftType<Int>.self)
/// types.assertTypesAreEqual()
/// ```
struct SwiftType<T> {}
struct SwiftTypePair<T1, T2> {
  init(typeOf: T1, type2: SwiftType<T2>.Type) {}

  mutating func assertTypesAreEqual() where T1 == T2 {}
}

// rdar://132064309
do {
  protocol P1<A, B> {
    associatedtype A
    associatedtype B
  }
  struct G<A> {}

  do {
    protocol P2 {
      associatedtype A
      func f1() -> any P1<G<A>, Int>
      func f() -> any P1<Int, G<A>>
    }

    let exist: any P2
    var types = SwiftTypePair(typeOf: exist.f(), type2: SwiftType<any P1>.self)
    types.assertTypesAreEqual()
  }

  do {
    protocol P2 {
      associatedtype A
      func f() -> any P1<Int, G<A>>
    }

    let exist: any P2
    var types = SwiftTypePair(typeOf: exist.f(), type2: SwiftType<any P1>.self)
    types.assertTypesAreEqual()
  }

  do {
    protocol P2 {
      associatedtype A: P1 where A.A == G<B>
      associatedtype B

      func f() -> any P1<G<A.A>, Int>
    }

    let exist: any P2
    var types = SwiftTypePair(typeOf: exist.f(), type2: SwiftType<any P1>.self)
    types.assertTypesAreEqual()
  }

  do {
    protocol P2 {
      associatedtype A: P1 where A.A == G<Int>
      func f() -> any P1<G<A.A>, Int>
    }

    let exist: any P2
    var types = SwiftTypePair(typeOf: exist.f(), type2: SwiftType<any P1<G<G<Int>>, Int>>.self)
    types.assertTypesAreEqual()
  }
}
