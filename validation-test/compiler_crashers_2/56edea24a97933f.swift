// {"signature":"swift::isRepresentableInObjC(swift::VarDecl const*, swift::ObjCReason)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a class b open extension b {
  @objc c : a
