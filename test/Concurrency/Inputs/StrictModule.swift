public struct StrictStruct: Hashable {
  public init() {}
}

open class StrictClass {
  open func send(_ body: @Sendable () -> Void) {}
  open func dontSend(_ body: () -> Void) {}
}

public protocol StrictProtocol {
  func send(_ body: @Sendable () -> Void)
  func dontSend(_ body: () -> Void)
}
