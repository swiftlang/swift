// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: 65015626.(file).G@
// CHECK-NEXT: Requirement signature: <Self where Self.Word : FixedWidthInteger, Self.Word == Self.Word.Magnitude>
public protocol G {
  associatedtype Word: FixedWidthInteger where Word.Magnitude == Word
}
