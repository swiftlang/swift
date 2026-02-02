// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Other.swiftmodule %S/Inputs/strict_access_control_other.swift -module-name Other -swift-version 5
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature StrictAccessControl -swift-version 5 -enable-library-evolution -I %t -verify-ignore-unrelated

// REQUIRES: swift_feature_StrictAccessControl

@_implementationOnly import Other
// expected-warning@-1 {{'@_implementationOnly' is deprecated, use 'internal import' instead}}

private struct PrivateType {}
// expected-note@-1 {{struct 'PrivateType' is not '@usableFromInline' or public}}

@inlinable public func localTypeAliasTest() {
  typealias Bad = PrivateType
  // expected-error@-1 {{struct 'PrivateType' is private and cannot be referenced from an '@inlinable' function}}
}

private protocol PrivateProtocol {}
// expected-note@-1 2{{type declared here}}

public typealias BadRequirements<T> = T where T: PrivateProtocol
// expected-error@-1 {{generic type alias cannot be declared public because its generic requirement uses a private type}}

public struct S {
  public subscript<T>(_: T) -> T where T: PrivateProtocol { fatalError() }
  // expected-error@-1 {{subscript cannot be declared public because its generic requirement uses a private type}}
}

@_spi(Testing)
public func badImplementationOnlyExport(_: ImplementationOnly) {}
// expected-error@-1 {{cannot use struct 'ImplementationOnly' here; 'Other' has been imported as implementation-only}}
