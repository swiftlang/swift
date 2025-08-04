// {"kind":"typecheck","signature":"diagnoseInvalidObjCName(swift::ValueDecl*, swift::ObjCAttr*)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  func 0.0
