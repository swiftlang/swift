// RUN: %target-swift-frontend %s -typecheck -verify

public protocol rdar28048391 {
  static func oops() -> Self?
}

extension rdar28048391 {
  public static func oops() -> Self? {
    return self
    // expected-error@-1 {{cannot convert return expression of type 'Self.Type' to return type 'Self?'}}
  }
}

extension ImplicitlyUnwrappedOptional : rdar28048391 { }
