// RUN: %target-swift-emit-silgen %s -verify -enable-upcoming-feature ApproachableConcurrency -default-isolation MainActor | %FileCheck %s

// REQUIRES: concurrency

struct S {
  func test() {
  }
}

func takesSendable<T: Sendable>(_: T) {}

// CHECK-LABEL: sil hidden [ossa] @$s24approachable_concurrency21testSendableInference1syAA1SV_tF : $@convention(thin) (S) -> ()
// CHECK: function_ref @$s24approachable_concurrency21testSendableInference1syAA1SV_tFyyYbcAEYbcfu_ : $@convention(thin) @Sendable (S) -> @owned @Sendable @callee_guaranteed () -> ()
// CHECK: } // end sil function '$s24approachable_concurrency21testSendableInference1syAA1SV_tF'
func testSendableInference(s: S) {
  takesSendable(s.test)
}

// CHECK-LABEL: sil hidden [ossa] @$s24approachable_concurrency25testNonisolatedNonSendingyyyyYaYCXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
func testNonisolatedNonSending(_: () async -> Void) async {
}

// GlobalActorIsolatedTypesUsability
@MainActor
struct GAITU {
  nonisolated var x: Int = 0
}

extension GAITU: Equatable {
  static nonisolated func ==(lhs: GAITU, rhs: GAITU) -> Bool {
    return lhs.x == rhs.x // okay
  }
}

// CHECK: // static IsolatedConformances.__derived_struct_equals(_:_:)
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-LABEL: sil hidden [ossa] @$s24approachable_concurrency20IsolatedConformancesV23__derived_struct_equalsySbAC_ACtFZ : $@convention(method) (IsolatedConformances, IsolatedConformances, @thin IsolatedConformances.Type) -> Bool
struct IsolatedConformances: Equatable {
  let x: Int = 0
}
