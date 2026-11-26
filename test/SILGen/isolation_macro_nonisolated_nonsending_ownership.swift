// RUN: %target-swift-emit-silgen -module-name test -target %target-swift-5.1-abi-triple %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -module-name test -target %target-swift-5.1-abi-triple %s -o /dev/null

// REQUIRES: concurrency

// In a `nonisolated(nonsending)` function, `#isolation` borrows the implicit
// isolated parameter. It has to be copied when it is consumed
// (https://github.com/swiftlang/swift/issues/92799).

func borrowIsolation(_ iso: (any Actor)?) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test17describeIsolationSSyYaF :
// CHECK:       bb0([[IMPLICIT_ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor):
// CHECK:         [[ACTOR:%.*]] = implicitactor_to_opaqueisolation_cast [[IMPLICIT_ACTOR]]
// CHECK:         [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]]
// CHECK:         [[ARG:%.*]] = alloc_stack $Optional<any Actor>
// CHECK:         store [[ACTOR_COPY]] to [init] [[ARG]]
// CHECK:         [[FN:%.*]] = function_ref @$sSS10describingSSx_tclufC :
// CHECK:         apply [[FN]]<Optional<any Actor>>([[ARG]], {{%.*}})
// CHECK:       } // end sil function '$s4test17describeIsolationSSyYaF'
nonisolated(nonsending) func describeIsolation() async -> String {
  String(describing: #isolation)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test13wrapIsolationScA_pSgSgyYaF :
// CHECK:       bb0([[IMPLICIT_ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor):
// CHECK:         [[ACTOR:%.*]] = implicitactor_to_opaqueisolation_cast [[IMPLICIT_ACTOR]]
// CHECK:         [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]]
// CHECK:         [[WRAPPED:%.*]] = enum $Optional<Optional<any Actor>>, #Optional.some!enumelt, [[ACTOR_COPY]]
// CHECK:         return [[WRAPPED]]
// CHECK:       } // end sil function '$s4test13wrapIsolationScA_pSgSgyYaF'
nonisolated(nonsending) func wrapIsolation() async -> (any Actor)?? {
  .some(#isolation)
}

// CHECK-LABEL: sil hidden [ossa] @$s4test14storeIsolationScA_pSgyYaF :
// CHECK:         [[ACTOR:%.*]] = implicitactor_to_opaqueisolation_cast
// CHECK:         [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]]
// CHECK:       } // end sil function '$s4test14storeIsolationScA_pSgyYaF'
nonisolated(nonsending) func storeIsolation() async -> (any Actor)? {
  let iso = #isolation
  return iso
}

//   A guaranteed argument can use the borrowed value directly.
// CHECK-LABEL: sil hidden [ossa] @$s4test23passIsolationAtPlusZeroyyYaF :
// CHECK:         [[ACTOR:%.*]] = implicitactor_to_opaqueisolation_cast
// CHECK-NOT:     copy_value
// CHECK:         [[FN:%.*]] = function_ref @$s4test15borrowIsolationyyScA_pSgF :
// CHECK-NEXT:    apply [[FN]]([[ACTOR]])
// CHECK:       } // end sil function '$s4test23passIsolationAtPlusZeroyyYaF'
nonisolated(nonsending) func passIsolationAtPlusZero() async {
  borrowIsolation(#isolation)
}
