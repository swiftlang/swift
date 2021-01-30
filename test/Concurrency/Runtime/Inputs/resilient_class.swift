public enum MyError : Error {
  case bad
}

open class BaseClass<T> {
  let value: T

  public init(value: T) {
    self.value = value
  }

  open func waitForNothing() async {}
  open func wait() async -> T {
    return value
  }
  open func wait(orThrow: Bool) async throws {
    if orThrow {
      throw MyError.bad
    }
  }
}
