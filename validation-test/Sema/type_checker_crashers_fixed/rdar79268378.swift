// RUN: not %target-swift-frontend %s -typecheck

struct Result {
}

func wrapper(_: Result?) {
}

extension Optional where Wrapped == Result {
  static func test(_: String) -> Result { Result() }
}

extension Result {
  static func test<R: RangeExpression>(_: R) -> Result where R.Bound == Int {
    Result()
  }
}

protocol P {}

struct Value : P {
  init() {}
  init<R>(_: R) {}
}

func example<T1: P, T2: P>(_: T1, _: T2) {
}

example(Value(), Value(wrapper(.test(0))))
