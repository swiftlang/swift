// RUN: not --crash %target-swift-frontend -typecheck %s

// SR-12642: Crash regarding `Differentiable` derived conformances and redeclared properties.

import _Differentiation

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

struct Generic<T> {}
extension Generic: Differentiable where T: Differentiable {}

struct WrappedProperties: Differentiable {
  @Wrapper var int: Generic<Int>
  @Wrapper var int: Generic<Int>
}
