@backDeployed(before: macOS 99)
@frozen public struct Point: Equatable {
  public var x: Int
  public init(x: Int) { self.x = x }
}

public final class NotSendable {}

@backDeployed(before: macOS 99)
@frozen public struct Holder {
  public var value: NotSendable
  public init(value: NotSendable) { self.value = value }
}
