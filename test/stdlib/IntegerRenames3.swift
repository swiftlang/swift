// RUN: %target-typecheck-verify-swift -swift-version 3

func f<T : SignedInteger>(x: T) {
  _ = T.addWithOverflow(x, x) // expected-error {{'addWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and addingReportingOverflow(_:) method instead.}}
  _ = T.subtractWithOverflow(x, x) // expected-error {{'subtractWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and subtractingReportingOverflow(_:) method instead.}}
  _ = T.multiplyWithOverflow(x, x) // expected-error {{'multiplyWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and multipliedReportingOverflow(by:) method instead.}}
  _ = T.divideWithOverflow(x, x) // expected-error {{'divideWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and dividedReportingOverflow(by:) method instead.}}
  _ = T.remainderWithOverflow(x, x) // expected-error {{'remainderWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and remainderReportingOverflow(dividingBy:) method instead.}}
}

func f<T : FixedWidthInteger>(x: T) {
  _ = T.addWithOverflow(x, x) // no error
  _ = T.subtractWithOverflow(x, x) // no error
  _ = T.multiplyWithOverflow(x, x) // no error
  _ = T.divideWithOverflow(x, x) // no error
  _ = T.remainderWithOverflow(x, x) // no error
}

do {
  let _: IntMax = 0 // no error
  let _: UIntMax = 0 // no error
}

func integer<T : Integer>(x: T) {} // no error
func integerArithmetic<T : IntegerArithmetic>(x: T) {} // no error
func signedNumber<T : SignedNumber>(x: T) {} // no error
func absoluteValuable<T : AbsoluteValuable>(x: T) {} // no error
func _signedInteger<T : _SignedInteger>(x: T) {} // no error

func absolutaValuable<T : SignedNumeric & Comparable>(x: T) {
  _ = T.abs(x) // no error
}

func signedIntegerMaskingArithmetics<T : SignedInteger>(x: T) {
  _ = x &+ x // no error
  _ = x &- x // no error
}
