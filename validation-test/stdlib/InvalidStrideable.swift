// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-executable -DTEST_EQUATABLE -o %t/InvalidStrideableEq %s
// RUN: ! %target-run %t/InvalidStrideableEq 2>&1 | %FileCheck %s --check-prefix=CHECK-EQUATABLE
// RUN: %target-build-swift -emit-executable -DTEST_COMPARABLE -o %t/InvalidStrideableCmp %s
// RUN: ! %target-run %t/InvalidStrideableCmp 2>&1 | %FileCheck %s --check-prefix=CHECK-COMPARABLE
// REQUIRES: executable_test

// FIXME: rdar://35780657
// UNSUPPORTED: swift_test_mode_optimize_size

//
// Check that a circular Strideable inheriting witnesses from Stdlib crashes
// with a rich error message.
//

struct InvalidStrideable : Strideable, SignedNumeric {
  typealias Magnitude = InvalidStrideable
  init?<T>(exactly: T) where T : BinaryInteger { return nil }
  var magnitude: InvalidStrideable { return self }

  static func += (lhs: inout InvalidStrideable, rhs: InvalidStrideable) { }
  static func -= (lhs: inout InvalidStrideable, rhs: InvalidStrideable) { }
  static func *= (lhs: inout InvalidStrideable, rhs: InvalidStrideable) { }

  static func + (lhs: InvalidStrideable, rhs: InvalidStrideable) -> InvalidStrideable { return rhs }
  static func - (lhs: InvalidStrideable, rhs: InvalidStrideable) -> InvalidStrideable { return rhs }
  static func * (lhs: InvalidStrideable, rhs: InvalidStrideable) -> InvalidStrideable { return rhs }

  typealias IntegerLiteralType = Int
  init(integerLiteral : Int) {}

  typealias Stride = InvalidStrideable

  init() {}

  func distance(to rhs: InvalidStrideable) -> InvalidStrideable { return self }
  func advanced(by n: InvalidStrideable) -> InvalidStrideable { return self }
}

#if TEST_EQUATABLE
  // CHECK-EQUATABLE: fatal error: Strideable conformance where 'Stride == Self' requires user-defined implementation of the '==' operator
  _ = InvalidStrideable() == InvalidStrideable()
#else
  // CHECK-COMPARABLE: fatal error: Strideable conformance where 'Stride == Self' requires user-defined implementation of the '<' operator
  _ = InvalidStrideable() < InvalidStrideable() // Will trap with error message
#endif
