// RUN: %target-typecheck-verify-swift -swift-version 5

private class PrivateType {} // expected-note {{class 'PrivateType' is not '@usableFromInline' or public}}
// expected-note@-1 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-note@-2 {{type declared here}}

@propertyWrapper
public struct Wrapper<T> {
  public init(wrappedValue: T) {}

  public var wrappedValue: T { fatalError() }
}

@frozen public struct UsesPrivateType {
  @Wrapper private var y1: PrivateType
  // expected-error@-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  @Wrapper private var y2 = PrivateType()
  // expected-error@-1 {{class 'PrivateType' is private and cannot be referenced from a property initializer in a '@frozen' type}}
  // expected-error@-2 {{initializer 'init()' is private and cannot be referenced from a property initializer in a '@frozen' type}}
  // expected-error@-3 {{type referenced from a stored property with inferred type 'PrivateType' in a '@frozen' struct must be '@usableFromInline' or public}}
}
