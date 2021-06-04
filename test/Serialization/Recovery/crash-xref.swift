/// Test xref error description by removing a type from a module after building
/// a client.
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/partials)
// RUN: %empty-directory(%t/normal)
// RUN: %empty-directory(%t/errors)

/// Compile module A with a type and an empty module B.
// RUN: %target-swift-frontend %s -emit-module-path %t/partials/A.swiftmodule -module-name A -D LIB
// RUN: %target-swift-frontend %s -emit-module-path %t/partials/B.swiftmodule -module-name B

/// Compile a client using the type from A.
// RUN: %target-swift-frontend %s -emit-module-path %t/normal/Client.swiftmodule -module-name Client -D CLIENT -I %t/partials
// RUN: %target-swift-frontend %s -emit-module-path %t/errors/Client.swiftmodule -module-name Client -D CLIENT -I %t/partials -experimental-allow-module-with-compiler-errors

/// Swap A and B around! A is now empty and B defines the type.
// RUN: %target-swift-frontend %s -emit-module-path %t/partials/A.swiftmodule -module-name A
// RUN: %target-swift-frontend %s -emit-module-path %t/partials/B.swiftmodule -module-name B -D LIB

/// Read from the client to get an xref error to the type missing from A.
// RUN: not --crash %target-swift-frontend -emit-sil %t/normal/Client.swiftmodule -module-name Client -I %t/partials -disable-deserialization-recovery 2> %t/normal_stderr
// RUN: not --crash %target-swift-frontend -emit-sil %t/errors/Client.swiftmodule -module-name Client -I %t/partials -disable-deserialization-recovery 2> %t/error_stderr

// RUN: cat %t/normal_stderr | %FileCheck %s -check-prefixes=NORMALFAILURE
// NORMALFAILURE-LABEL: *** DESERIALIZATION FAILURE ***
// NORMALFAILURE-NEXT: module 'Client' with full misc version {{.*}}'
// NORMALFAILURE-NEXT: Could not deserialize type for 'foo()'
// NORMALFAILURE-NEXT: Caused by: top-level value not found
// NORMALFAILURE-NEXT: Cross-reference to module 'A'
// NORMALFAILURE-NEXT: ... SomeType
// NORMALFAILURE-NEXT: Notes:
// NORMALFAILURE-NEXT: * There is a matching 'SomeType' in module 'B'. If this is imported from clang, please make sure the header is part of a single clang module.

// RUN: cat %t/error_stderr | %FileCheck %s -check-prefixes=ALLOWFAILURE
// ALLOWFAILURE-LABEL: *** DESERIALIZATION FAILURE ***
// ALLOWFAILURE-NEXT: module 'Client' with full misc version {{.*}}' (built with -experimental-allow-module-with-compiler-errors)
// ALLOWFAILURE-NEXT: Could not deserialize type for 'foo()'
// ALLOWFAILURE-NEXT: Caused by: top-level value not found
// ALLOWFAILURE-NEXT: Cross-reference to module 'A'
// ALLOWFAILURE-NEXT: ... SomeType
// ALLOWFAILURE-NEXT: Notes:
// ALLOWFAILURE-NEXT: * There is a matching 'SomeType' in module 'B'. If this is imported from clang, please make sure the header is part of a single clang module.

#if LIB
public struct SomeType {
    public init() {}
}
#elseif CLIENT
import A
import B

public func foo() -> A.SomeType { fatalError() }
#endif // Empty
