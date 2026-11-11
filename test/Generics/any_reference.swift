// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AnyReference -debug-generic-signatures %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_AnyReference

class C {}
protocol ClassP: AnyObject {}

// CHECK-LABEL: .takesAnyReference@
// CHECK-NEXT: Generic signature: <T where T : _AnyReference>
func takesAnyReference<T: _AnyReference>(_: T) {}

// CHECK-LABEL: .withAnyObject@
// CHECK-NEXT: Generic signature: <T where T : AnyObject>
func withAnyObject<T: _AnyReference>(_: T) where T: AnyObject {}

// CHECK-LABEL: .withAnyObjectFirst@
// CHECK-NEXT: Generic signature: <T where T : AnyObject>
func withAnyObjectFirst<T: AnyObject>(_: T) where T: _AnyReference {}

// CHECK-LABEL: .withSuperclass@
// CHECK-NEXT: Generic signature: <T where T : C>
func withSuperclass<T: _AnyReference>(_: T) where T: C {}

// CHECK-LABEL: .withClassProtocol@
// CHECK-NEXT: Generic signature: <T where T : ClassP>
func withClassProtocol<T: _AnyReference>(_: T) where T: ClassP {}

// CHECK-LABEL: .RefP@
// CHECK-NEXT: Requirement signature: <Self where Self : _AnyReference>
protocol RefP: _AnyReference {}

struct Generic<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Generic
// CHECK-NEXT: Generic signature: <T where T : _AnyReference>
extension Generic where T: _AnyReference {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Generic
// CHECK-NEXT: Generic signature: <T where T == C>
extension Generic where T == C, T: _AnyReference {}
