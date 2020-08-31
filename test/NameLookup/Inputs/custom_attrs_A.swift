@propertyWrapper
public struct Wrapper<Value> {
  public var wrappedValue: Value

  public init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

@_functionBuilder
public struct Builder {
  static func buildBlock<T>(_ component: T) -> T { component }
}
