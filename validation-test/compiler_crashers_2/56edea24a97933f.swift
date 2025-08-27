// {"kind":"typecheck","signature":"swift::isRepresentableInObjC(swift::VarDecl const*, swift::ObjCReason)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a class b open extension b {
  @objc c : a
