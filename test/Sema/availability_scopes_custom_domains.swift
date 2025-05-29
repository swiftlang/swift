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

// FIXME: [availability] Should be "available=A"
// CHECK: {{^}}  (decl version={{.*}} deprecated decl=deprecatedInA()
@available(A, deprecated)
func deprecatedInA() { }
