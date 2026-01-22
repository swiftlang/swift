// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/NormalLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-public-helper.swift \
// RUN:  -swift-version 5
// RUN: %target-swift-frontend -emit-module -o %t/BADLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-helper.swift -I %t \
// RUN:  -swift-version 5

/// Warnings by default
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %t \
// RUN:  -swift-version 6 -verify-additional-prefix not-opt-in-

/// Opt-in errors
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %t \
// RUN:  -swift-version 6 -verify-additional-prefix opt-in- \
// RUN:  -enable-experimental-feature CheckImplementationOnly

// REQUIRES: swift_feature_CheckImplementationOnly

import NormalLibrary
@_implementationOnly import BADLibrary // expected-not-opt-in-warning {{safely use '@_implementationOnly' without library evolution by setting '-enable-experimental-feature CheckImplementationOnly' for 'main'}}

/// No diagnostics on functions in non-embedded mode.

internal typealias NormalProtoAssoc<T: NormalProto> = T.Assoc
internal func testConformanceInTypealias(_: NormalProtoAssoc<NormalStruct>) {}

internal struct NormalProtoAssocHolder<T: NormalProto> {
  public var value: T.Assoc
}
internal func testConformanceInBoundGeneric(_: NormalProtoAssocHolder<NormalStruct>) {}

internal struct OuterGenericHolder<T> {
  internal struct Nested where T : NormalProto {
    public var value: T.Assoc
  }
}
internal func testConformanceInNestedNonGeneric(_: OuterGenericHolder<NormalStruct>.Nested) {}

internal class SubclassOfNormalClass: NormalClass {}

internal func testInheritedConformance(_: NormalProtoAssocHolder<SubclassOfNormalClass>) {}
internal func testSpecializedConformance(_: NormalProtoAssocHolder<GenericStruct<Int>>) {}

extension Array where Element == NormalProtoAssocHolder<NormalStruct> { // Should we error here?
  internal func testConstrainedExtensionUsingBadConformance() {}
}

public struct ConditionalGenericStruct<T> {}
extension ConditionalGenericStruct: NormalProto where T: NormalProto {
  public typealias Assoc = Int
}
internal func testConditionalGeneric(_: NormalProtoAssocHolder<ConditionalGenericStruct<NormalStruct>>) {}

public protocol PublicAssociatedTypeProto {
  associatedtype Assoc: NormalProto
  func takesAssoc(_: Assoc)
}
@usableFromInline protocol UFIAssociatedTypeProto {
  associatedtype Assoc: NormalProto
  func takesAssoc(_: Assoc)
}
protocol InternalAssociatedTypeProto {
  associatedtype Assoc: NormalProto
  func takesAssoc(_: Assoc)
}

public struct PublicInferredAssociatedTypeImpl {
  public func takesAssoc(_: NormalStruct) {}
}
extension PublicInferredAssociatedTypeImpl: PublicAssociatedTypeProto {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note@-1 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension PublicInferredAssociatedTypeImpl: UFIAssociatedTypeProto {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note@-1 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension PublicInferredAssociatedTypeImpl: InternalAssociatedTypeProto {} // okay

@usableFromInline struct UFIInferredAssociatedTypeImpl {
  public func takesAssoc(_: NormalStruct) {}
}
extension UFIInferredAssociatedTypeImpl: PublicAssociatedTypeProto {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note@-1 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension UFIInferredAssociatedTypeImpl: UFIAssociatedTypeProto {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note@-1 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension UFIInferredAssociatedTypeImpl: InternalAssociatedTypeProto {} // okay

struct InternalInferredAssociatedTypeImpl {
  public func takesAssoc(_: NormalStruct) {}
}
extension InternalInferredAssociatedTypeImpl: PublicAssociatedTypeProto {}
// expected-not-opt-in-warning @-1 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-opt-in-error @-2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note @-3 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}
extension InternalInferredAssociatedTypeImpl: UFIAssociatedTypeProto {}
// expected-not-opt-in-warning @-1 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-opt-in-error @-2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note @-3 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension InternalInferredAssociatedTypeImpl: InternalAssociatedTypeProto {} // okay

internal struct PublicExplicitAssociatedTypeImpl {
  internal typealias Assoc = NormalStruct
  internal func takesAssoc(_: NormalStruct) {}
}
extension PublicExplicitAssociatedTypeImpl: PublicAssociatedTypeProto {}
// expected-not-opt-in-warning @-1 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-opt-in-error @-2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note@-3 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension PublicExplicitAssociatedTypeImpl: UFIAssociatedTypeProto {}
// expected-not-opt-in-warning @-1 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-opt-in-error @-2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
// expected-note@-3 {{in associated type 'Self.Assoc' (inferred as 'NormalStruct')}}

extension PublicExplicitAssociatedTypeImpl: InternalAssociatedTypeProto {} // okay
