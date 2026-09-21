// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000

// This used to type check quickly in 6.2 and regressed in 6.3.
// https://github.com/swiftlang/swift/issues/90116

@_disfavoredOverload
func __checkBinaryOperation<T, U>(_: T, _: (T, () -> U) -> Bool, _: @autoclosure () -> U) {}

@_disfavoredOverload
func __checkBinaryOperation<T>(_: T, _: (T, () -> T) -> Bool, _: @autoclosure () -> T)
    where T: BidirectionalCollection, T.Element: Equatable {}

func __checkBinaryOperation(_: String, _: (String, () -> String) -> Bool, _: @autoclosure () -> String) {}

func __checkBinaryOperation<T, U>(_: T, _: (T, () -> U) -> Bool, _: @autoclosure () -> U)
    where T: RangeExpression, U: RangeExpression {}

@_disfavoredOverload
func __checkBinaryOperation<T>(_: T?, _: (T?, () -> T?) -> T?, _: @autoclosure () -> T?) {}

func f() {
  __checkBinaryOperation([],{ $0 == $1() },["" + "" + "", "" + "" + ""])
  // expected-error@-1 {{reasonable time}}
}
