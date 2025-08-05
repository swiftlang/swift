// {"kind":"typecheck","signature":"swift::InFlightDiagnostic swift::diagnoseAttrWithRemovalFixIt<swift::Diag<>&>(swift::Decl const*, swift::DeclAttribute const*, swift::Diag<>&)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc class b
