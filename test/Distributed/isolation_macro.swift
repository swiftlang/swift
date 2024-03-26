// RUN: %target-swift-frontend -typecheck %s -verify

// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser

import Distributed

@available(SwiftStdlib 5.7, *)
extension DistributedActor {
  // CHECK-LABEL: actorIsolationToSelf()
  func actorIsolationToSelf() {
    // CHECK: macro_expansion_expr
    // CHECK: rewritten=current_context_isolation_expr
    // CHECK-NEXT: inject_into_optional
    // CHECK: member_ref_expr{{.*}}asLocalActor
    // CHECK: declref_expr type="Self"
    _ = #isolation
  }
}

// CHECK-LABEL: actorIsolationToParam(_:)
@available(SwiftStdlib 5.7, *)
func actorIsolationToParam(_ isolatedParam: isolated any DistributedActor) {
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK: member_ref_expr{{.*}}asLocalActor
  _ = #isolation
}

func acceptClosure(_ body: () -> Void) { }

// CHECK-LABEL: closureIsolatedToOuterParam(
@available(SwiftStdlib 5.7, *)
func closureIsolatedToOuterParam(_ isolatedParam: isolated any DistributedActor) {
  // CHECK: closure_expr
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK: inject_into_optional
  // CHECK: member_ref_expr{{.*}}asLocalActor
  acceptClosure {
    _ = #isolation
    print(isolatedParam)
  }
}
