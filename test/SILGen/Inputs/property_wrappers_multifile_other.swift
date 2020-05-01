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
