// RUN: %target-swift-frontend -dump-ast %s -enable-experimental-feature OptionalIsolatedParameters | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_swift_parser

// CHECK-LABEL: nonisolatedFunc()
@available(SwiftStdlib 5.1, *)
func nonisolatedFunc() {
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK-NEXT: nil_literal_expr
  _ = #isolation
}

// CHECK-LABEL: nonisolatedAsyncFunc()
@available(SwiftStdlib 5.1, *)
func nonisolatedAsyncFunc() async {
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK-NEXT: nil_literal_expr
  _ = #isolation
}

@available(SwiftStdlib 5.1, *)
actor A {
}

@available(SwiftStdlib 5.1, *)
extension A {
  // CHECK-LABEL: actorIsolationToSelf()
  func actorIsolationToSelf() {
    // CHECK: macro_expansion_expr
    // CHECK: rewritten=current_context_isolation_expr
    // CHECK-NEXT: inject_into_optional
    // CHECK-NEXT: erasure_expr
    // CHECK: declref_expr type="A"{{.*}}self@
    _ = #isolation
  }
}

// CHECK-LABEL: actorIsolationToParam(_:)
@available(SwiftStdlib 5.1, *)
func actorIsolationToParam(_ isolatedParam: isolated A) {
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK-NEXT: inject_into_optional
  // CHECK-NEXT: erasure_expr
  // CHECK: declref_expr type="A"{{.*}}isolatedParam@
  _ = #isolation
}

// CHECK-LABEL: mainActorIsolated()
@available(SwiftStdlib 5.1, *)
@MainActor
func mainActorIsolated() {
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK-NEXT: inject_into_optional
  // CHECK-NEXT: erasure_expr
  // CHECK: member_ref_expr type="MainActor" location=@__swiftmacro_{{.*}} decl="_Concurrency.(file).MainActor.shared"
  // CHECK-NEXT: type_expr type="MainActor.Type"
  _ = #isolation
}

func acceptClosure(_ body: () -> Void) { }

// CHECK-LABEL: closureIsolatedToOuterParam(
@available(SwiftStdlib 5.1, *)
func closureIsolatedToOuterParam(_ isolatedParam: isolated A) {
  // CHECK: closure_expr
  // CHECK: macro_expansion_expr
  // CHECK: rewritten=current_context_isolation_expr
  // CHECK-NEXT: inject_into_optional
  // CHECK-NEXT: erasure_expr
  // CHECK: declref_expr type="A"{{.*}}isolatedParam@
  acceptClosure {
    _ = #isolation
    print(isolatedParam)
  }
}

func acceptEscapingClosure(_ fn: @escaping () -> Void) { }

@available(SwiftStdlib 5.1, *)
extension A {
  func f() {
    // Make sure this doesn't diagnose a use of implicit 'self'
    acceptEscapingClosure {
      _ = #isolation
      self.g()
    }
  }

  func g() {}
}
