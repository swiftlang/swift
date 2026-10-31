import Observation

@available(SwiftStdlib 5.9, *)
@Observable
public final class ObservableUsableFromInline<Value> {
  @usableFromInline var value: Value

  public init(value: Value) {
    self.value = value
  }

  @inlinable
  public func get() -> Value {
    value
  }

  @inlinable
  public func set(_ newValue: Value) {
    value = newValue
  }
}
