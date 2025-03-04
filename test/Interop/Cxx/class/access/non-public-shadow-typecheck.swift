// RUN: %target-swift-frontend %s -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// REQUIRES: swift_feature_ImportNonPublicCxxMembers
import NonPublicShadow

func f(s: Shadow) {
  // publOrPriv() and protOrPriv() will respectively resolve to
  // Publ::publOrPriv() and Prot::protOrPriv() because the ambiguous results in
  // Priv are marked @unavailable, which take lower priority in Swift's name
  // lookup rules.
  s.publOrPriv()
  s.protOrPriv() // expected-error {{'protOrPriv' is inaccessible due to 'private' protection level}}

  // TODO: this should complain about ambiguity, but it doesn't
  // s.publOrProt()

  // These all work in the derived class because they shadow the base member
  // irrespective of the base member's access levl
  s.publPublShadowed()
  s.protPublShadowed()
  s.privPublShadowed()

  // These all complain about 'private' because they shadow the base member
  // irrespective of the base member's access levl
  s.publPrivShadowed() // expected-error {{'publPrivShadowed' is inaccessible due to 'private' protection level}}
  s.protPrivShadowed() // expected-error {{'protPrivShadowed' is inaccessible due to 'private' protection level}}
  s.privPrivShadowed() // expected-error {{'privPrivShadowed' is inaccessible due to 'private' protection level}}
}
