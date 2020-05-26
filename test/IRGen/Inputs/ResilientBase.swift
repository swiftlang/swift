@propertyWrapper
public struct Wrapper<Value> {
  var value: Value

  public init(wrappedValue: Value) {
    self.init(initialValue: wrappedValue)
  }

  public init(initialValue: Value) {
    value = initialValue
  }

  public var wrappedValue: Value {
    get { fatalError("unreachable") }
    set { fatalError("unreachable") }
  }

  public static subscript<T: AnyObject>(
    _enclosingInstance object: T,
    wrapped wrappedKeyPath: ReferenceWritableKeyPath<T, Value>,
    storage storageKeyPath: ReferenceWritableKeyPath<T, Self>
  ) -> Value {
    get {
      object[keyPath: storageKeyPath].value
    }
    set {
      object[keyPath: storageKeyPath].value = newValue
    }
  }
}

open class ResilientBase {
  public init() {}
}
