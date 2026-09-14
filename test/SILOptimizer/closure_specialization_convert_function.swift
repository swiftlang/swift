// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name CS -Xllvm -sil-print-types %s | %FileCheck %s

// Normalizing function conversions must enable closure specialization in the optimization pipeline.

public class A {}
public class B: A {}

@inline(never)
public func make() -> B { B() }

// CHECK-LABEL: {{^}}// specialized use(_:)
// CHECK:       sil shared [noinline] @$s2CS3useyAA1ACADyXEF17$s2CS4makeAA1BCyFTf1c_n : $@convention(thin) () -> @owned A {
// CHECK:       bb0:
// CHECK:         %[[#S0:]] = function_ref @$s2CS4makeAA1BCyF : $@convention(thin) () -> @owned B
// CHECK:         %[[#S1:]] = apply %[[#S0]]() : $@convention(thin) () -> @owned B
// CHECK:         %[[#S2:]] = upcast %[[#S1]] : $B to $A
// CHECK:         return %[[#S2]] : $A
// CHECK-NEXT:  }
@inline(never)
public func use(_ f: () -> A) -> A { f() }

// CHECK-LABEL: sil @$s2CS4testAA1AC_ADtyF :
// CHECK:       bb0:
// CHECK:         %[[#C0:]] = function_ref @$s2CS3useyAA1ACADyXEF17$s2CS4makeAA1BCyFTf1c_n : $@convention(thin) () -> @owned A
// CHECK:         %[[#C1:]] = apply %[[#C0]]() : $@convention(thin) () -> @owned A
// CHECK:         %[[#C2:]] = apply %[[#C0]]() : $@convention(thin) () -> @owned A
// CHECK:         %[[#C3:]] = tuple (%[[#C1]] : $A, %[[#C2]] : $A)
// CHECK:         return %[[#C3]] : $(A, A)
// CHECK-NEXT:  }
public func test() -> (A, A) {
  let first = use(make)
  let f = make
  return (first, use(f))
}
