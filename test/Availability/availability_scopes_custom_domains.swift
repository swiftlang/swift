// RUN:  %target-swift-frontend -typecheck %s \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain A \
// RUN:  -define-enabled-availability-domain B \
// RUN:  -dump-availability-scopes > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace %s < %t.dump

// REQUIRES: swift_feature_CustomAvailability

// CHECK: {{^}}(root version={{.*}}
// CHECK: {{^}}  (decl version={{.*}} available=A decl=availableInA()
@available(A)
func availableInA() { }

// CHECK: {{^}}  (decl version={{.*}} unavailable=A decl=unavailableInA()
@available(A, unavailable)
func unavailableInA() { }

// CHECK: {{^}}  (decl version={{.*}} available=A,B decl=availableInAB()
@available(A)
@available(B)
func availableInAB() { }

// CHECK: {{^}}  (decl version={{.*}} decl=deprecatedInA()
@available(A, deprecated)
func deprecatedInA() { }

// CHECK: {{^}}  (condition_following_availability version={{.*}} available=A
// CHECK-NEXT: {{^}}  (if_then version={{.*}} available=A
// CHECK-NEXT: {{^}}    (decl_implicit version={{.*}} available=A decl=unannotatedFuncInIfThen()
// CHECK-NEXT: {{^}}      (decl version={{.*}} available=A decl=unannotatedFuncInIfThen()
// CHECK-NEXT: {{^}}    (decl_implicit version={{.*}} available=A decl=availableInBFuncInIfThen()
// CHECK-NEXT: {{^}}      (decl version={{.*}} available=A,B decl=availableInBFuncInIfThen()
// CHECK-NEXT: {{^}}    (decl_implicit version={{.*}} available=A decl=availableInAFuncInIfThen()
// CHECK-NEXT: {{^}}      (decl version={{.*}} available=A decl=availableInAFuncInIfThen()
// CHECK-NEXT: {{^}}    (decl_implicit version={{.*}} available=A decl=unavailableInAFuncInIfThen()
// CHECK-NEXT: {{^}}      (decl version={{.*}} unavailable=A decl=unavailableInAFuncInIfThen()
// CHECK-NEXT: {{^}}    (decl_implicit version={{.*}} available=A decl=AvailableInBStructInIfThen
// CHECK-NEXT: {{^}}      (decl version={{.*}} available=A,B decl=AvailableInBStructInIfThen
// CHECK-NEXT: {{^}}        (decl version={{.*}} available=B unavailable=A decl=unavailableInAMethod()
// CHECK-NEXT: {{^}}  (condition_following_availability version={{.*}} available=A
// CHECK-NEXT: {{^}}  (guard_else version={{.*}} unavailable=A
// CHECK-NEXT: {{^}}  (guard_fallthrough version={{.*}} available=A
// CHECK-NEXT: {{^}}    (decl_implicit version={{.*}} available=A decl=funcInGuardFallthrough()
// CHECK-NEXT: {{^}}      (decl version={{.*}} available=A,B decl=funcInGuardFallthrough()

func localDeclsInAvailabilityScopes() {
  if #available(A) {
    func unannotatedFuncInIfThen() { }

    @available(B)
    func availableInBFuncInIfThen() { }

    @available(A)
    func availableInAFuncInIfThen() { }

    @available(A, unavailable)
    func unavailableInAFuncInIfThen() { }

    @available(B)
    struct AvailableInBStructInIfThen {
      func method() { }

      @available(A, unavailable)
      func unavailableInAMethod() { }
    }
  }

  guard #available(A) else { return }

  @available(B)
  func funcInGuardFallthrough() { }
}
