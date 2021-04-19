/// Test xref error description by removing a type from a module after building
/// a client.
// RUN: %empty-directory(%t)

/// Compile module A with a type and an empty module B.
// RUN: %target-swift-frontend %s -emit-module-path %t/A.swiftmodule -module-name A -D LIB
// RUN: %target-swift-frontend %s -emit-module-path %t/B.swiftmodule -module-name B
#if LIB
public struct SomeType {
    public init() {}
}

/// Compile a client using the type from A.
// RUN: %target-swift-frontend %s -emit-module-path %t/Client.swiftmodule -module-name Client -D CLIENT -I %t
#elseif CLIENT
import A
import B

public func foo() -> A.SomeType { fatalError() }

/// Swap A and B around! A is now empty and B defines the type.
// RUN: %target-swift-frontend %s -emit-module-path %t/A.swiftmodule -module-name A
// RUN: %target-swift-frontend %s -emit-module-path %t/B.swiftmodule -module-name B -D LIB
#endif // Empty

/// Read from the client to get an xref error to the type missing from A.
// RUN: not --crash %target-swift-frontend -emit-sil %t/Client.swiftmodule -module-name Client -I %t -disable-deserialization-recovery 2> %t/stderr

// RUN: cat %t/stderr | %FileCheck %s
// CHECK: *** DESERIALIZATION FAILURE (please include this section in any bug report) ***
// CHECK-NEXT: Could not deserialize type for 'foo()'
// CHECK-NEXT: Caused by: top-level value not found
// CHECK-NEXT: Cross-reference to module 'A'
// CHECK-NEXT: ... SomeType
// CHECK-NEXT: Notes:
// CHECK-NEXT: * There is a matching 'SomeType' in module 'B'. If this is imported from clang, please make sure the header is part of a single clang module.
