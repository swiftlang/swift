public class MyClass {
  public init() { }

  @PropertyWrapper(key: "key", defaultValue: false)
  public static var wrappedProperty: Bool

  public var otherProperty: String? {
    didSet {
      fatalError("Set this other property with value: \(String(describing: otherProperty)), even though we called `myClass.property = `")
    }
  }

  public var property: String? {
    didSet {
      print("Set expected property: \(String(describing: property))")
    }
  }
}

@propertyWrapper
public struct PropertyWrapper<Value> {
  public let key: String
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

  public init(key: String, defaultValue: Value) {
    self.key = key
    self.defaultValue = defaultValue
  }
}
