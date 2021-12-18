// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: rdar83955123.(file).Cyclo@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.L.R, Self.D : Cyclo, Self.L : Cyclo, Self.R : Cyclo, Self.U : Cyclo, Self.L.R == Self.U.D>
protocol Cyclo {
  associatedtype L: Cyclo
  associatedtype R: Cyclo
  associatedtype U: Cyclo
  associatedtype D: Cyclo
      where L.R == Self,
            U.D == Self
}
