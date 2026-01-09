// RUN: %target-swift-ide-test -swift-version 5 -print-indexed-symbols -source-filename %s | %FileCheck %s

// Test that appendInterpolation methods are referenced from string interpolation
// (https://github.com/swiftlang/swift/issues/56189)

extension DefaultStringInterpolation {
  // CHECK: [[@LINE+1]]:17 | instance-method(internal)/Swift | appendInterpolation(value:) | [[appendInterpolation_value_USR:.*]] | Def
  mutating func appendInterpolation(value: Int) {}
}

func testStringInterpolation() {
  // CHECK: [[@LINE+1]]:11 | instance-method/Swift | appendInterpolation(value:) | [[appendInterpolation_value_USR]] | Ref,Call,Impl
  print("\(value: 1)")

  // CHECK: [[@LINE+1]]:18 | instance-method/Swift | appendInterpolation(value:) | [[appendInterpolation_value_USR]] | Ref,Call,Impl
  print("value: \(value: 42)")
}
