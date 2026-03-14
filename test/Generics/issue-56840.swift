// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/56840

// CHECK-LABEL: .P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]Input == Self.[P]Output.[Numeric]Magnitude, Self.[P]Output : FixedWidthInteger, Self.[P]Output : SignedInteger>
protocol P {
  associatedtype Input: FixedWidthInteger & UnsignedInteger & BinaryInteger
  associatedtype Output: FixedWidthInteger & SignedInteger & BinaryInteger
    where Output.Magnitude == Input
}

