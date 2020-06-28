// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx10.15 %s
// REQUIRES: OS=macosx

enum Either<T,U> {
  case first(T)
  case second(U)
}

struct Do<T> {
  var value: T
}

@_functionBuilder
struct TupleBuilder {
  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }

  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T1>(_ t1: T1) -> Do<(T1)> {
    .init(value: t1)
  }

  static func buildDo<T1, T2>(_ t1: T1, _ t2: T2) -> Do<(T1, T2)> {
    .init(value: (t1, t2))
  }
  
  static func buildDo<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> Do<(T1, T2, T3)> {
    .init(value: (t1, t2, t3))
  }

  static func buildIf<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }

  static func buildArray<T>(_ array: [T]) -> [T] { return array }
}

@available(macOS 10.14, *)
enum Option {
  @available(macOS 10.15.4, *)
  case best
}

@TupleBuilder
func bestTuple() -> some Any { // expected-note{{add @available attribute to enclosing global function}}
  "Hello"
  Option.best // expected-error{{'best' is only available in macOS 10.15.4 or newer}}
  // expected-note@-1{{add 'if #available' version check}}
}

func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

tuplify(true) { x in
  x
  "Hello"
  Option.best // expected-error{{'best' is only available in macOS 10.15.4 or newer}}
  // expected-note@-1{{add 'if #available' version check}}
}
