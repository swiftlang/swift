// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -module-name isolated_any -emit-module -o %t/isolated_any.swiftmodule -emit-module-interface-path - %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -module-name isolated_any -emit-module -o %t/isolated_any.swiftmodule -emit-module-interface-path - %s | %FileCheck %s

// CHECK: {{^}}public func test1(fn: @isolated(any) @Sendable () -> ())
public func test1(fn: @isolated(any) @Sendable () -> ()) {}

// CHECK-NEXT: {{^}}public func test2(fn: @isolated(any) @Sendable () -> ())
@_allowFeatureSuppression(XXX)
public func test2(fn: @isolated(any) @Sendable () -> ()) {}

// CHECK-NEXT: {{^}}public func test3(fn: @isolated(any) @Sendable () -> ())
@_allowFeatureSuppression(IsolatedAny)
public func test3(fn: @isolated(any) @Sendable () -> ()) {}
