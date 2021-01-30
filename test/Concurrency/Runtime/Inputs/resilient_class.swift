open class BaseClass<T> {
  let value: T

  public init(value: T) {
    self.value = value
  }

  open func waitForNothing() async {}
  open func wait() async -> T {
    return value
  }
}
