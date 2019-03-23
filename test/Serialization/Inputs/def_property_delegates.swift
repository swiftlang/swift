@propertyDelegate
public struct SomeDelegate<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }
}

public struct HasDelegates {
  public var x: Int by SomeDelegate
  public var y: Int by public SomeDelegate
  public private(set) var z: Int by public SomeDelegate
}
