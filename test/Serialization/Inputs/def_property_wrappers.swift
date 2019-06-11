@_propertyWrapper
public struct SomeWrapper<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }
}

public struct HasWrappers {
  @SomeWrapper public var x: Int = 17
}
