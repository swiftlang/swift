// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name Hidden -emit-module-path %t/Hidden.swiftmodule \
// RUN:   %t/Hidden.swift
// RUN: %target-swift-frontend -typecheck -parse-as-library \
// RUN:   -module-name Test -I %t -dump-client-declaration-requirements \
// RUN:   %t/Test.swift 2>&1 \
// RUN:   | %FileCheck %s

// CHECK: Client declaration requirements for module 'Test':
// CHECK-DAG: Test.PublicType: FullDeclaration
// CHECK-DAG: Test.PublicType.publicMethod(): FullDeclaration
// CHECK-DAG: Test.PublicType.usableFromInlineMethod(): FullDeclaration
// CHECK-DAG: Test.PublicType.internalMethod(): None
// CHECK-DAG: Test.InternalType: None
// CHECK-DAG: Test.PrivateType: None
// CHECK-DAG: Hidden.IdentityOnly: OpaqueDeclaration
// CHECK-DAG: Hidden.Layout: AbstractTypeLayoutDeclaration

//--- Hidden.swift

public struct IdentityOnly {}
public struct Layout {}

//--- Test.swift

@_implementationOnly import Hidden

public struct PublicType {
  public func publicMethod() {}
  @usableFromInline
  internal func usableFromInlineMethod() {}
  internal func internalMethod() {}
}

internal struct InternalType {}
private struct PrivateType {}

public struct PublicStorage {
  private var callback: (IdentityOnly) -> Void
  private var value: Layout
}
