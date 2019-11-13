@propertyWrapper
public struct Wrapper<Value> {
  public var wrappedValue: Value

  public init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}
