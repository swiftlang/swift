// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -module-name Test -enable-library-evolution -experimental-lazy-typecheck -verify

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
