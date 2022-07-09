public struct Logger {
  public init() {}
  @_spi(Usable) public func spiFunc() {}
  public func regularFunc() {}
  func internalFunc() {}
  private func privateFunc() {}
}
struct InternalLogger {
  init() {}
}
private struct PrivateLogger {
  init() {}
}
public func setup() -> XLogging.Logger? {
  return Logger()
}
