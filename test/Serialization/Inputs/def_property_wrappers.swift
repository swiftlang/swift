public struct OtherWrapper<T> {
  var value: T
}

@propertyWrapper
public struct SomeWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue initialValue: T) {
    self.wrappedValue = initialValue
  }

  public var projectedValue: OtherWrapper<T> {
    OtherWrapper(value: wrappedValue)
  }
}

public struct HasWrappers {
  @SomeWrapper public var x: Int = 17
}

// SR-10844
@propertyWrapper
class A<T: Equatable> {

  private var _value: T

  var wrappedValue: T {
    get { _value }
    set { _value = newValue }
  }

  init(wrappedValue initialValue: T) {
    _value = initialValue
  }
}

@propertyWrapper
class B: A<Double> {
  override var wrappedValue: Double {
    get { super.wrappedValue }
    set { super.wrappedValue = newValue }
  }
}

class Holder {
  // @A var b = 10.0 // ok
  @B var b = 10.0 // crash in test target
}
