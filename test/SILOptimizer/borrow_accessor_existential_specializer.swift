// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all -sil-stop-optzns-before-lowering-ownership -module-name test %s | %FileCheck %s

// ExistentialSpecializer turns a borrow accessor with an existential parameter
// into a thunk that calls a generic specialization. The @guaranteed result of
// that call is a borrow scope, so the thunk returns it with a return_borrow.

public protocol P {
  var isValid: Bool { get }
}

struct S: P {
  var isValid: Bool { true }
}

public final class Klass {}

public struct Container {
  var k: Klass

  public init(_ k: Klass) { self.k = k }

  // CHECK-LABEL: sil [signature_optimized_thunk] [heuristic_always_inline] [ossa] @$s4test9ContainerVyAA5KlassCAA1P_pcib : $@convention(method) (@in_guaranteed any P, @guaranteed Container) -> @guaranteed Klass {
  // CHECK:       bb0(%0 : $*any P, %1 : @guaranteed $Container):
  // CHECK:         [[F:%.*]] = function_ref @$s4test9ContainerVyAA5KlassCAA1P_pcibTf4en_n :
  // CHECK:         [[O:%.*]] = open_existential_addr immutable_access %0
  // CHECK:         [[R:%.*]] = apply [[F]]<@opened({{.*}}) Self>([[O]], %1)
  // CHECK-NEXT:    return_borrow [[R]] from_scopes ([[R]])
  // CHECK-LABEL: } // end sil function '$s4test9ContainerVyAA5KlassCAA1P_pcib'
  public subscript(key: any P) -> Klass {
    @inline(never)
    borrow {
      precondition(key.isValid)
      return k
    }
  }
}

@inline(never)
func use(_ k: Klass) {}

// After the thunk is inlined, the caller calls the specialization for S and
// ends the borrow scope of its result.
//
// CHECK-LABEL: sil [ossa] @$s4test13callSubscriptyyAA9ContainerVF :
// CHECK:         [[F:%.*]] = function_ref @$s4test9ContainerVyAA5KlassCAA1P_pcibTf4en_nAA1SV_Tg5 :
// CHECK:         [[R:%.*]] = apply [[F]]({{%.*}}, %0)
// CHECK:         end_borrow [[R]]
// CHECK-LABEL: } // end sil function '$s4test13callSubscriptyyAA9ContainerVF'
public func callSubscript(_ c: Container) {
  use(c[S()])
}
