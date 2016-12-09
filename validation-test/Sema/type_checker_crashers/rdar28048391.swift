// RUN: not --crash %target-swift-frontend %s -typecheck
// REQUIRES: asserts

public protocol rdar28048391 {
  static func oops() -> Self?
}

extension rdar28048391 {
  public static func oops() -> Self? {
    return self
  }
}

extension ImplicitlyUnwrappedOptional : rdar28048391 { }
