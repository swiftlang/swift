// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: rdar83955123.(file).Cyclo@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Cyclo]L.[Cyclo]R, Self.[Cyclo]D : Cyclo, Self.[Cyclo]L : Cyclo, Self.[Cyclo]R : Cyclo, Self.[Cyclo]U : Cyclo, Self.[Cyclo]L.[Cyclo]R == Self.[Cyclo]U.[Cyclo]D>
protocol Cyclo {
  associatedtype L: Cyclo
  associatedtype R: Cyclo
  associatedtype U: Cyclo
  associatedtype D: Cyclo
      where L.R == Self,
            U.D == Self
}
