// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: 65015626.(file).G@
// CHECK-NEXT: Requirement signature: <Self where Self.[G]Word : FixedWidthInteger, Self.[G]Word == Self.[G]Word.[Numeric]Magnitude>
public protocol G {
  associatedtype Word: FixedWidthInteger where Word.Magnitude == Word
}
