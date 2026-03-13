// RUN: %target-swift-frontend -emit-sil -O -unavailable-decl-optimization=stub %s

public struct S {}

@available(*, unavailable)
public struct Unavailable {
  public let i = Self.j

  static let j = S()
}
