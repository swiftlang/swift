// RUN: %target-swift-frontend -typecheck %s -verify -enable-library-evolution

@propertyWrapper
public struct ResilientWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T, description: String) {
    self.wrappedValue = wrappedValue
  }
}

func getHello() -> String { return "hello" } // expected-note {{global function 'getHello()' is not '@usableFromInline' or public}}

@frozen
public struct StructUsesPublishedAsPrivate {
  public var integer: Int = 17

  @ResilientWrapper(description: getHello()) // expected-error {{global function 'getHello()' is internal and cannot be referenced from a property initializer in a '@frozen' type}}
  var otherString: String = "World"
}
