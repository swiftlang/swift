public class MyClass {
  public init() { }

  @PropertyWrapper()
  public var instanceProperty: Bool
}

@propertyWrapper
public struct PropertyWrapper {
  public init() { }

  public var projectedValue: PropertyWrapper {
    get {
      return self
    }
    set {
      self = newValue
    }
  }

  public var wrappedValue: Bool {
    return false
  }
}

@propertyWrapper
public struct ParameterWrapper<Value> {
  public var wrappedValue: Value

  public init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: ParameterWrapper<Value> { self }

  public init(projectedValue: ParameterWrapper<Value>) {
    self = projectedValue
  }
}

public func takesWrappedParameter(@ParameterWrapper value: Int) {
  _ = { value }
}
