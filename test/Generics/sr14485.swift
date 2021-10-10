// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr14485.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self.Input == Self.Output.Magnitude, Self.Output : FixedWidthInteger, Self.Output : SignedInteger>
protocol P {
  associatedtype Input: FixedWidthInteger & UnsignedInteger & BinaryInteger
  associatedtype Output: FixedWidthInteger & SignedInteger & BinaryInteger
    where Output.Magnitude == Input
}

