@_propertyDelegate
public struct SomeDelegate<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }
}

public struct HasDelegates {
  @SomeDelegate public var x: Int = 17
}
