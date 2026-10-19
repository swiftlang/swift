public protocol Stage {
  associatedtype FailureReason
}

// Larger than the direct-return limit, so the typed error goes through the
// caller-allocated indirect error slot.
public struct StageFailure<S: Stage>: Error {
  public var reason: S.FailureReason
  public var a: String
  public var b: String
  public var c: String
}

public struct MyStage<Output>: Stage {
  public typealias FailureReason = String
  public init() {}

  public func run() async throws(StageFailure<Self>) {
    throw StageFailure(reason: "could not run", a: "a", b: "b", c: "c")
  }
}
