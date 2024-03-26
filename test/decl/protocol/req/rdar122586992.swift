// RUN: %target-typecheck-verify-swift

public protocol P1 {
  associatedtype A
  func f() -> A
}

public struct S1 {}

extension Int: P1 {
  public func f() -> S1 { fatalError() }
}

public protocol P2: RawRepresentable, P1 {}

public struct S2 {}

extension P2 where RawValue == S2, A == S2 {
  // This is not a candidate witness, because we require A == S1.
  public func f() -> A { fatalError() }
}

extension P2 where RawValue: P1, RawValue.A == A {
  // This is a candidate witness.
  public func f() -> A { fatalError() }
}

public protocol P3: P2 where A == S1, RawValue == Int {}

// When checking [S3: P1], the fact that P3 has [A == S1] should completely
// solve everything.
public struct S3: P3 {
  public init(rawValue: Int) { fatalError() }
  public var rawValue: Int { fatalError() }
}

let x: S1.Type = S3.A.self