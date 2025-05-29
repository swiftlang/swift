// RUN: not %target-swift-frontend %s -typecheck

public protocol P1 {
  associatedtype A

  init(a: A)
}

public protocol P2: P1 {
  associatedtype B

  init()
}

extension P2 where A == B {
  public init() { fatalError() }
  public init(a: B) { fatalError() }
}

public protocol P3: P2 {
  associatedtype B = Self

  static var y: B { get }
}

public struct S: P3 {
  public static let x = S(c: 1)
  public static let y = S(c: 2)

  public init(c: Int) {}
}
