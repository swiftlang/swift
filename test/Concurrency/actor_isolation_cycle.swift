// RUN: %target-typecheck-verify-swift

public protocol P {
  associatedtype T
  @MainActor func f(_: T)
  @MainActor func g(_: T)
}
public struct S : P {
  public func g(_: Int) {}
  public func f(_: T) {}
}
