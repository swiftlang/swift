public class MyClass {
  public init() { }

  @PropertyWrapper(defaultValue: false)
  public var wrappedProperty: Bool

  public func check() {
    let foo = $wrappedProperty
    $wrappedProperty = foo
  }
}

@propertyWrapper
public struct PropertyWrapper<Value> {
  public let defaultValue: Value

  public var projectedValue: PropertyWrapper<Value> {
    get {
      return self
    }
    set {
      self = newValue
    }
  }

  public var wrappedValue: Value {
    return defaultValue
  }

  public init(defaultValue: Value) {
    self.defaultValue = defaultValue
  }
}
