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

// SR-10844
@_propertyWrapper
class A<T: Equatable> {

  private var _value: T

  var value: T {
    get { _value }
    set { _value = newValue }
  }

  init(initialValue: T) {
    _value = initialValue
  }
}

@_propertyWrapper
class B: A<Double> {
  override var value: Double {
    get { super.value }
    set { super.value = newValue }
  }
}

class Holder {
  // @A var b = 10.0 // ok
  @B var b = 10.0 // crash in test target
}
