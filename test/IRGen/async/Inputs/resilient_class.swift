open class BaseClass<T> {
  var value: T

  open func wait() async -> T {
    return value
  }

  public init(_ value: T) {
    self.value = value
  }
}
