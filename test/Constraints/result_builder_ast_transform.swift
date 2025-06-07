// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -I %t -L %t %s -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

@propertyWrapper
struct Wrapper<Value> {
  public var value: Value

  init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  var projectedValue: Self { return self }

  var wrappedValue: Value {
    get { self.value }
    set { self.value = newValue }
  }

  func test() -> Int { 42 }
}

extension Wrapper where Value: ExpressibleByNilLiteral {
  init() {
    self.value = nil
  }
}

enum Either<T,U> {
  case first(T)
  case second(U)
}

@resultBuilder
struct TupleBuilder {
  static func buildBlock() -> () { }

  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
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

  static func buildDo<T>(_ value: T) -> T { return value }
  static func buildOptional<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
}

func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

tuplify(true) { cond in
  @Wrapper var x: Int?
  x
  x = 42
  x
}
// CHECK: (nil, (), Optional(42))

tuplify(true) { cond in
  @Wrapper var x: Int = 42
  x
  if cond {
    $x
  }
}
// CHECK: (42, Optional(main.Wrapper<Swift.Int>(value: 42)))

tuplify(true) { cond in
  @Wrapper(wrappedValue: 42) var x: Int

  if cond && x == 42 {
    x = 30
    $x
  }

  x
}
// CHECK: (Optional(((), main.Wrapper<Swift.Int>(value: 30))), 30)

tuplify(true) { cond in
  if cond {
    @Wrapper(wrappedValue: 42) var x: Int
    if $x.test() > 0 {
      x
    }
  }

  ""
}
// CHECK: (Optional(Optional(42)), "")

tuplify(true) { cond in
  var x: Int?

  if cond {
    var y: Int?
    x = 1
    y = x
    y
  } else {
    x = 0
  }

  x
}
// CHECK: (main.Either<((), (), Swift.Optional<Swift.Int>), ()>.first((), (), Optional(1)), Optional(1))

tuplify(true) { cond in
  ""

  var x: Int {
    get { 42 }
  }

  if cond {
    x
  }

  ""
}
// CHECK: ("", Optional(42), "")

tuplify(true) { cond in
  lazy var x: Int = {
    42
  }()

  if cond {
    x
    x = 0
    x
  }

  ""
}
// CHECK: (Optional((42, (), 0)), "")
