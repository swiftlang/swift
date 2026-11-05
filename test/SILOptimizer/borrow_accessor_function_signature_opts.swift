// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all -module-name test %s | %FileCheck %s

// FunctionSignatureOpts removes the dead index of a borrow accessor, and turns
// the original accessor into a thunk that calls the specialization. In the
// performance pipeline, FunctionSignatureOpts runs after OSSA is lowered, so
// the thunk returns the result with a plain return.
// borrow_accessor_function_signature_opts.sil tests the OSSA return_borrow.

public final class Klass {}

public struct Container {
  var k: Klass

  public init(_ k: Klass) { self.k = k }

  // CHECK-LABEL: sil [signature_optimized_thunk] [heuristic_always_inline] @$s4test9ContainerVyAA5KlassCSicib : $@convention(method) (Int, @guaranteed Container) -> @guaranteed Klass {
  // CHECK:       bb0(%0 : $Int, %1 : $Container):
  // CHECK:         [[F:%.*]] = function_ref @$s4test9ContainerVyAA5KlassCSicibTf4dn_n : $@convention(method) (@guaranteed Container) -> @guaranteed Klass
  // CHECK:         [[R:%.*]] = apply [[F]](%1)
  // CHECK-NEXT:    return [[R]]
  // CHECK-LABEL: } // end sil function '$s4test9ContainerVyAA5KlassCSicib'
  public subscript(unused: Int) -> Klass {
    @inline(never)
    borrow {
      return k
    }
  }
}

@inline(never)
func use(_ k: Klass) {}

// After the thunk is inlined, the caller calls the specialization.
//
// CHECK-LABEL: sil @$s4test13callSubscriptyyAA9ContainerVF :
// CHECK:         [[F:%.*]] = function_ref @$s4test9ContainerVyAA5KlassCSicibTf4dn_n :
// CHECK:         apply [[F]](%0)
// CHECK-LABEL: } // end sil function '$s4test13callSubscriptyyAA9ContainerVF'
public func callSubscript(_ c: Container) {
  use(c[27])
}
