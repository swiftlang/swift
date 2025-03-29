
// dynamically_replaceable
@MainActor
public dynamic func dynamicOnMainActor() { }

// property wrapper + main actor
@propertyWrapper
public struct IntWrapper: Sendable {

  public init(wrappedValue: Int) {
    self.wrappedValue = wrappedValue
  }

  @MainActor public var wrappedValue: Int
}
