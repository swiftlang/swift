// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

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

