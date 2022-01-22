// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-pairwise-build-block) | %FileCheck %s
// REQUIRES: executable_test

struct Values<T> {
  var values: T

  init(values: T) {
    self.values = values
  }

  func map<R>(_ f: (T) -> R) -> Values<R> {
    .init(values: f(values))
  }
}

@resultBuilder
enum NestedTupleBuilder {
  static func buildBlock<T>(_ x: T) -> Values<T> {
    .init(values: x)
  }

  static func buildBlock<T, U>(
    combining next: U, into combined: Values<T>
  ) -> Values<(T, U)> {
    .init(values: (combined.values, next))
  }
}

extension Values {
  init(@NestedTupleBuilder nested values: () -> Self) {
    self = values()
  }
}

let nestedValues = Values(nested: {
  1
  "2"
  3.0
  "yes"
})
print(nestedValues)

// CHECK: Values<(((Int, String), Double), String)>(values: (((1, "2"), 3.0), "yes"))

@resultBuilder
enum FlatTupleBuilder {
  static func buildExpression<T>(_ x: T) -> Values<T> {
    .init(values: x)
  }

  static func buildBlock<T>(_ x: Values<T>) -> Values<T> {
    .init(values: x.values)
  }

  static func buildBlock<T, N>(
    combining new: Values<N>,
    into combined: Values<T>
  ) -> Values<(T, N)> {
    .init(values: (combined.values, new.values))
  }

  static func buildBlock<T0, T1, N>(
    combining new: Values<N>,
    into combined: Values<(T0, T1)>
  ) -> Values<(T0, T1, N)> {
    .init(values: (combined.values.0, combined.values.1, new.values))
  }

  static func buildBlock<T0, T1, T2, N>(
    combining new: Values<N>,
    into combined: Values<(T0, T1, T2)>
  ) -> Values<(T0, T1, T2, N)> {
    .init(values: (combined.values.0, combined.values.1, combined.values.2, new.values))
  }

  static func buildBlock<T0, T1, T2, T3, N>(
    combining new: Values<N>,
    into combined: Values<(T0, T1, T2, T3)>
  ) -> Values<(T0, T1, T2, T3, N)> {
    .init(values: (combined.values.0, combined.values.1, combined.values.2, combined.values.3, new.values))
  }

  static func buildBlock(_ x: Never...) -> Values<()> {
    assert(x.isEmpty, "I should never be called unless it's nullary")
    return .init(values: ())
  }

  static func buildEither<T>(first: T) -> T {
    first
  }

  static func buildEither<T>(second: T) -> T {
    second
  }

  static func buildOptional<T>(_ x: Values<T>?) -> Values<T?> {
    x?.map { $0 } ?? .init(values: nil)
  }

  static func buildLimitedAvailability<T>(_ x: Values<T>) -> Values<T> {
    x
  }
}

extension Values {
  init(@FlatTupleBuilder flat values: () -> Self) {
    self = values()
  }
}

let flatValues0 = Values(flat: {})
print(flatValues0)
// CHECK: Values<()>(values: ())

let flatValues1 = Values(flat: {
  1
  "2"
  3.0
})
print(flatValues1)
// CHECK: Values<(Int, String, Double)>(values: (1, "2", 3.0))

let flatValues2 = Values(flat: {
  1
  "2"
  let y = 3.0 + 4.0
  #if false
  "not gonna happen"
  #endif
  if true {
    "yes"
  } else {
    "no"
  }
  #warning("Beware of pairwise block building")
  #if true
  if false {
    "nah"
  }
  if #available(*) {
    5.0
  }
  #endif
})
print(flatValues2)

// CHECK: Values<(Int, String, String, Optional<String>, Optional<Double>)>(values: (1, "2", "yes", nil, Optional(5.0)))

struct Nil: CustomStringConvertible {
  var description: String {
    "nil"
  }
}
struct Cons<Head, Tail>: CustomStringConvertible {
  var head: Head
  var tail: Tail

  var description: String {
    "(cons \(String(reflecting: head)) \(tail))"
  }
}

@resultBuilder
enum ListBuilder {
  static func buildBlock() -> Nil {
    Nil()
  }

  static func buildBlock<T>(_ x: T) -> Cons<T, Nil> {
    .init(head: x, tail: Nil())
  }

  static func buildBlock<New, T>(combining new: New, into combined: T) -> Cons<New, T> {
    .init(head: new, tail: combined)
  }

  static func buildBlock<T>(_ x: T...) -> [T] {
    fatalError("I should never be called!")
  }
}

func list<T>(@ListBuilder f: () -> T) -> T {
  f()
}

let list0 = list {}
print(list0)
// CHECK: nil

let list1 = list { "1" }
print(list1)
// Check: (cons 1 nil)

let list2 = list {
  1
  2
}
print(list2)
// CHECK: (cons 2 (cons 1 nil))
let list3 = list {
  1
  list {
    2.0
    "3"
  }
  "4"
}
print(list3)
// CHECK: (cons "4" (cons (cons "3" (cons 2.0 nil)) (cons 1 nil)))
