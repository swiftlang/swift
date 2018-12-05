// RUN: %target-typecheck-verify-swift -swift-version 4

func f<T : SignedInteger>(x: T) {
  _ = T.addWithOverflow(x, x) // expected-error {{'addWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and addingReportingOverflow(_:) method instead.}}
  _ = T.subtractWithOverflow(x, x) // expected-error {{'subtractWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and subtractingReportingOverflow(_:) method instead.}}
  _ = T.multiplyWithOverflow(x, x) // expected-error {{'multiplyWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and multipliedReportingOverflow(by:) method instead.}}
  _ = T.divideWithOverflow(x, x) // expected-error {{'divideWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and dividedReportingOverflow(by:) method instead.}}
  _ = T.remainderWithOverflow(x, x) // expected-error {{'remainderWithOverflow' is unavailable: Please use FixedWidthInteger protocol as a generic constraint and remainderReportingOverflow(dividingBy:) method instead.}}
}

func f<T : FixedWidthInteger>(x: T) {
  _ = T.addWithOverflow(x, x) // expected-error {{'addWithOverflow' is unavailable: Use addingReportingOverflow(_:) instead.}}
  _ = T.subtractWithOverflow(x, x) // expected-error {{'subtractWithOverflow' is unavailable: Use subtractingReportingOverflow(_:) instead.}}
  _ = T.multiplyWithOverflow(x, x) // expected-error {{'multiplyWithOverflow' is unavailable: Use multipliedReportingOverflow(by:) instead.}}
  _ = T.divideWithOverflow(x, x) // expected-error {{'divideWithOverflow' is unavailable: Use dividedReportingOverflow(by:) instead.}}
  _ = T.remainderWithOverflow(x, x) // expected-error {{'remainderWithOverflow' is unavailable: Use remainderReportingOverflow(dividingBy:) instead.}}
}

do {
  let _: IntMax = 0 // expected-error {{'IntMax' has been renamed to 'Int64'}}
  let _: UIntMax = 0 // expected-error {{'UIntMax' has been renamed to 'UInt64'}}
}

func absolutaValuable<T : SignedNumeric & Comparable>(x: T) {
  _ = T.abs(x) // expected-error {{type 'T' has no member 'abs'}}
}

func signedIntegerMaskingArithmetics<T : SignedInteger>(x: T) {
  _ = x &+ x // expected-error {{use 'FixedWidthInteger' instead of 'SignedInteger' to get '&+' in generic code}}
  _ = x &- x // expected-error {{use 'FixedWidthInteger' instead of 'SignedInteger' to get '&-' in generic code}}
}
