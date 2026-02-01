// {"kind":"typecheck","original":"4818e0ef","signature":"swift::constraints::ConstraintSystem::simplifyConformsToConstraint(swift::Type, swift::ProtocolDecl*, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>)","signatureAssert":"Assertion failed: (!empty()), function back"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
protocol a: NSObject
  func b(c: a.Type) {
    &[: , a ]...[c
