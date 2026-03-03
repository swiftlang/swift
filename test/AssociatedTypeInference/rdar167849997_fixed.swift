// RUN: %target-typecheck-verify-swift

// This is the "corrected" version of the test case from
// rdar167849997.swift.

let x = G<Int, String>.A.self
let y = G<Int, String>.B.self

let xx: Int.Type = x
let yy: String.Type = y

public protocol P {
  associatedtype A
  associatedtype B

  func f(_: A, _: B)
}

public extension P {
    typealias B = Void
}

public struct G<A, B>: P {
  // FIXME: This should not be necessary
  public typealias B = B

  public func f(_: A, _: B) {
    fatalError()
  }
}
