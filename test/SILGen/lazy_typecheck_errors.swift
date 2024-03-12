// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck -verify
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck -experimental-skip-non-inlinable-function-bodies -verify
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck -experimental-skip-non-inlinable-function-bodies -experimental-skip-non-exportable-decls -verify


public protocol Proto {
  func req()
}

public struct ConformsToProtoMissingRequirement: Proto {
  // expected-error@-1 {{type 'ConformsToProtoMissingRequirement' does not conform to protocol 'Proto'}}
}

public struct ConformsToProtoNearMiss: Proto {
  // expected-error@-1 {{type 'ConformsToProtoNearMiss' does not conform to protocol 'Proto'}}

  public func req(x: Int) {}
}

public protocol ProtoWithAssociatedType {
  associatedtype A
}

public struct ConformsToProtoProtoWithAssociatedType: ProtoWithAssociatedType {
  // expected-error@-1 {{type 'ConformsToProtoProtoWithAssociatedType' does not conform to protocol 'ProtoWithAssociatedType'}}
}

public struct GenericStruct<T> {
  public var t: T
}

public struct GenericStructWithInvalidParameterConstraint<T: NonExistent> {
  // expected-error@-1 {{cannot find type 'NonExistent' in scope}}
}

extension GenericStruct where T == NonExistent {
  // expected-error@-1 {{cannot find type 'NonExistent' in scope}}
  public func methodInExtensionWithInvalidWhereClause() {}
}

public class ValidClass {
  public func methodReturningNonExistentType() -> NonExistent {}
  // expected-error@-1 {{cannot find type 'NonExistent' in scope}}
}

public class DerivedFromValidClass: ValidClass {
  override public func methodReturningNonExistentType() -> NonExistent {}
  // expected-error@-1 {{cannot find type 'NonExistent' in scope}}
}

public class DerivedFromNonExistent: NonExistent {
  // expected-error@-1 {{cannot find type 'NonExistent' in scope}}

  public func method() {}
}
