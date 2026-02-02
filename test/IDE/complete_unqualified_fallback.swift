// RUN: %batch-code-completion

protocol P0 {}
protocol P1 {}
protocol P2 {}
protocol P3 {}
protocol P4 {}
protocol P5 {}
protocol P6 {}
protocol P7 {}
protocol P8 {}
protocol P9 {}

struct FooBar: P0 {}

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ x: T) -> T { x }
  static func buildExpression<T: P0>(_ x: T) -> T { x }
  static func buildExpression<T: P1>(_ x: T) -> T { x }
  static func buildExpression<T: P2>(_ x: T) -> T { x }
  static func buildExpression<T: P3>(_ x: T) -> T { x }
  static func buildExpression<T: P4>(_ x: T) -> T { x }
  static func buildExpression<T: P5>(_ x: T) -> T { x }
  static func buildExpression<T: P6>(_ x: T) -> T { x }
  static func buildExpression<T: P7>(_ x: T) -> T { x }
  static func buildExpression<T: P8>(_ x: T) -> T { x }
  static func buildExpression<T: P9>(_ x: T) -> T { x }
}

struct S<T> {}
extension S: P0 where T : P0 { init(@Builder fn: () -> T) {} }
extension S: P1 where T : P1 { init(@Builder fn: () -> T) {} }
extension S: P2 where T : P2 { init(@Builder fn: () -> T) {} }
extension S: P3 where T : P3 { init(@Builder fn: () -> T) {} }
extension S: P4 where T : P4 { init(@Builder fn: () -> T) {} }
extension S: P5 where T : P5 { init(@Builder fn: () -> T) {} }
extension S: P6 where T : P6 { init(@Builder fn: () -> T) {} }
extension S: P7 where T : P7 { init(@Builder fn: () -> T) {} }
extension S: P8 where T : P8 { init(@Builder fn: () -> T) {} }
extension S: P9 where T : P9 { init(@Builder fn: () -> T) {} }

// This is currently too complex, make sure we can still do an unqualified
// lookup though.
S {
  S {
    S {
      S {
        S {
          #^COMPLETE^#
          // COMPLETE: Decl[Struct]/CurrModule: FooBar[#FooBar#]; name=FooBar
        }
      }
    }
  }
}
