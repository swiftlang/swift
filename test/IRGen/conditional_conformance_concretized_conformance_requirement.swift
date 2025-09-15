// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/swiftlang/swift/issues/76479

public struct G<T> where T: P1, T.A: P1, T.A.A: P1 {}

public protocol P1 {
  associatedtype A
}

extension Int: P1 {
  public typealias A = Int
}

public protocol P2 {
  func f()
}

extension G: P2 where T.A == Int {
  public func f() {}
}
