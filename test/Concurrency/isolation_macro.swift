// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

// Diagnostics testing
// RUN: not %target-swift-frontend -swift-version 5 -typecheck -DTEST_DIAGNOSTICS %s > %t/diagnostics.txt 2>&1
// RUN: %FileCheck %s --check-prefix CHECK-DIAGS < %t/diagnostics.txt

// REQUIRES: concurrency
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
  // CHECK-NEXT: type_expr implicit type="MainActor.Type"
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

#if TEST_DIAGNOSTICS
@available(SwiftStdlib 5.1, *)
actor ConcreteActor {}

@available(SwiftStdlib 5.1, *)
func concreteActorIsolation(
  actor: isolated ConcreteActor = #isolation
) async {}

@available(SwiftStdlib 5.1, *)
@MainActor
func testContextualType() {
  let _: any Actor = #isolation
  let _: MainActor = #isolation
  let _: MainActor? = #isolation

  // CHECK-DIAGS: error: cannot convert value of type 'MainActor' to expected argument type 'Int'
  // CHECK-DIAGS: note: in expansion of macro 'isolation' here
  // CHECK-DIAGS: let _: Int = #isolation
  let _: Int = #isolation

  // CHECK-DIAGS: error: cannot convert value of type 'MainActor' to expected argument type 'ConcreteActor'
  // CHECK-DIAGS: note: in expansion of macro 'isolation' here
  // CHECK-DIAGS: await concreteActorIsolation()
  await concreteActorIsolation()
}
#endif

func isolationMacroDefault(
  isolation: isolated (any Actor)? = #isolation,
) async -> Void {}

class C {
  @globalActor
  actor NestedActor {
    static let shared = NestedActor()
  }

  @NestedActor
  func expandIsolation() async {
    await isolationMacroDefault()
  }
}
