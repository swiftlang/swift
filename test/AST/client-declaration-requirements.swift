// RUN: %target-swift-frontend -typecheck -parse-as-library \
// RUN:   -module-name Test -dump-client-declaration-requirements %s 2>&1 \
// RUN:   | %FileCheck %s

// CHECK: Client declaration requirements for module 'Test':
// CHECK-DAG: Test.PublicType: FullDeclaration
// CHECK-DAG: Test.PublicType.publicMethod(): FullDeclaration
// CHECK-DAG: Test.PublicType.usableFromInlineMethod(): FullDeclaration
// CHECK-DAG: Test.PublicType.internalMethod(): None
// CHECK-DAG: Test.InternalType: None
// CHECK-DAG: Test.PrivateType: None

public struct PublicType {
  public func publicMethod() {}
  @usableFromInline
  internal func usableFromInlineMethod() {}
  internal func internalMethod() {}
}

internal struct InternalType {}
private struct PrivateType {}
