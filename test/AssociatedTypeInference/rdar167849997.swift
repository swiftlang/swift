// RUN: %target-typecheck-verify-swift

// This was reduced from rdar://167849997. The original code is actually
// incorrect, because the user wrote 'Self.B' instead of 'B' in the
// implementation of G, and a qualified lookup there will find the type
// alias in the protocol extension, and not the generic parameter of
// 'G' named 'B'.
//
// Thus, we would infer the type witness for 'B' to be '()' in the
// 'G: P' conformance, and not the generic parameter
// as was probably intended.

let x = G<Int, String>.A.self
let y = G<Int, String>.B.self

let xx: Int.Type = x
let yy: ().Type = y

public protocol P {
  associatedtype A
  associatedtype B

  func f(_: A, _: B)
}

public extension P {
    typealias B = Void
}

public struct G<A, B>: P {
  // Note: Self.B
  public func f(_: A, _: Self.B) {
    fatalError()
  }
}
