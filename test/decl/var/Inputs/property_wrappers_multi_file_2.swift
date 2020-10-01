@propertyWrapper
public struct WrapGod<T> {
  private var value: T
  
  public init(wrappedValue: T) {
    value = wrappedValue
  }
  
  public var wrappedValue: T {
    get { value }
    set { value = newValue }
  }
  
  public var projectedValue: T {
    get { value }
    set { value = newValue }
  }
}

class BaseClass {
  @WrapGod final var value: Int = 42
  init() {}
}
