// RUN: %target-swift-emit-silgen -verify %s
func orElse<T: ~Copyable>(
    x: consuming T?,
    defaultValue: @autoclosure () throws -> T?
) rethrows -> T? {
  switch x {
  case .some(let value):
    return value // expected-warning{{returning the non-'Copyable' value of a pattern binding from a switch that borrows by default}}
  case .none:
    return try defaultValue()
  }
}
