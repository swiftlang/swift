// RUN: %target-typecheck-verify-swift

// SR-13183: Make sure we don't incorrectly split the constraint system without
// considering that a function builder type var may connect the inside of a
// closure body with the enclosing expression.

struct New<Value> {
  init(value: Value, @ScopeBuilder<Value> scope: () -> Component) { }
}

struct Component {}

struct Map<Value, Transformed> {
  let transform: (Value) -> Transformed
}

@_functionBuilder
struct ScopeBuilder<Value> {
  static func buildExpression<T>(_ map: Map<Value, T>) -> Component {
    Component()
  }

  static func buildBlock(_ components: Component...) -> Component {
    Component()
  }
}

let new1 = New(value: 42) {
  Map { $0.description }
}

let new2 = New<Int>(value: 42) {
  Map { $0.description }
}
