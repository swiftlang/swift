// rdar://125445665
// RUN: %target-typecheck-verify-swift

protocol Keyframes<Value> {
  associatedtype Value
}

struct Track<Value>: Keyframes {
  init<T>(_ keyPath: WritableKeyPath<Value, T>) {}
}

struct Pair<Value>: Keyframes {}

struct Conditional<First: Keyframes, Second: Keyframes>: Keyframes
  where First.Value == Second.Value {
  typealias Value = First.Value
}

@resultBuilder
struct KeyframesBuilder<Value> {
  static func buildExpression<K: Keyframes>(_ expression: K) -> K
    where K.Value == Value { expression }
  static func buildPartialBlock<K: Keyframes>(first: K) -> K
    where K.Value == Value { first }
  static func buildPartialBlock(
    accumulated: some Keyframes<Value>, next: some Keyframes<Value>
  ) -> some Keyframes<Value> { Pair<Value>() }
  static func buildEither<First: Keyframes, Second: Keyframes>(
    first: First
  ) -> Conditional<First, Second> where First.Value == Value {
    Conditional()
  }
  static func buildEither<First: Keyframes, Second: Keyframes>(
    second: Second
  ) -> Conditional<First, Second> where Second.Value == Value {
    Conditional()
  }
}

func animate<Value>(
  initialValue: Value,
  @KeyframesBuilder<Value> keyframes: (Value) -> some Keyframes<Value>
) {}

struct Values {
  var scale = 1.0
}

func test() {
  animate(initialValue: Values()) { _ in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    if true {
      Track(\.scale)
    }
    if false {
      Track(\.scale)
    }
  }
}
