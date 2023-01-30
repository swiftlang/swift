// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/ReflectionMetadata.swiftinterface) %s -module-name CRM -enable-experimental-feature RuntimeDiscoverableAttrs
// RUN: %target-swift-typecheck-module-from-interface(%t/ReflectionMetadata.swiftinterface) -module-name CRM
// RUN: %FileCheck %s < %t/ReflectionMetadata.swiftinterface

// REQUIRES: asserts

@runtimeMetadata
public struct Flag {
  public init<T>(attachedTo: T) {}
}

@Flag
public class BaseClass {}

// CHECK: public class TestClass : CRM.BaseClass

public class TestClass : BaseClass {}

// CHECK: @available(*, unavailable)
// CHECK-NEXT: @Flag extension CRM.TestClass {
// CHECK-NEXT: }

@available(*, unavailable)
@Flag
public extension TestClass {
}
