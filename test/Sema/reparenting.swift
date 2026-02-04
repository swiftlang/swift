// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

// REQUIRES: objc_interop
// REQUIRES: swift_feature_Reparenting

@reparentable
protocol Q {}

@reparentable
protocol R {}

protocol P {}
extension P : @reparented Q {}

protocol P2 {}
extension P2 : @reparented Q, @reparented R {}

protocol DirectlyOnProto: @reparented R {}
// expected-error@-1 {{'@reparented' only applies in inheritance clause of a protocol extension}}

class Whatever: @reparented AnyObject {}
// expected-error @-1 {{'@reparented' only applies in inheritance clause of a protocol extension}}

protocol Invalid {
  associatedtype A: @reparented Q
  // expected-error @-1 {{'@reparented' only applies in inheritance clause of a protocol extension}}
}

extension Invalid: @reparented AnyObject {}
// expected-error @-1 {{'@reparented' cannot apply to non-protocol type 'AnyObject'}}

extension Invalid: @reparented Whatever {}
// expected-error @-1 {{'@reparented' cannot apply to non-protocol type 'Whatever'}}

extension Invalid: @reparented P {}
// expected-error @-1 {{'@reparented' cannot apply to 'P' unless it is declared to be '@reparentable'}}

extension Invalid: @reparented Q & R {}
// expected-error @-1 {{'@reparented' cannot apply to non-protocol type 'Q & R'}}

@objc protocol ObjCProto {}
extension ObjCProto: @reparented Q {}
// expected-error @-1 {{@objc protocol 'ObjCProto' cannot be '@reparented'}}



// The reparentable protocol shouldn't directly inherit from some other protocol that requires a witness table.
public protocol GrandParentJiji {
  associatedtype Element
}
@reparentable
public protocol ParentJiji: GrandParentJiji {} // expected-note {{'Jiji' does not conform to inherited protocol 'GrandParentJiji'}}
public protocol Jiji {
  associatedtype Element
}
extension Jiji: @reparented ParentJiji {} // expected-error {{'Jiji' cannot conform to 'GrandParentJiji'}}
// expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}


public protocol Pumpkin {}
@reparentable public protocol AnyObjParent: AnyObject {}
@reparentable public protocol ActorParent: Actor {}
@reparentable public protocol SendableParent: Sendable {}

extension Pumpkin: @reparented AnyObjParent {} // expected-error {{non-class type 'Pumpkin' cannot conform to class protocol 'AnyObjParent'}}
extension Pumpkin: @reparented ActorParent {}  // expected-error {{non-class type 'Pumpkin' cannot conform to class protocol 'ActorParent'}}
extension Pumpkin: @reparented SendableParent {}
