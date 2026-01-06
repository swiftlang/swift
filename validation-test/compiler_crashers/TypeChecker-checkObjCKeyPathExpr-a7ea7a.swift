// {"kind":"typecheck","signature":"swift::TypeChecker::checkObjCKeyPathExpr(swift::DeclContext*, swift::KeyPathExpr*, bool)","signatureAssert":"Assertion failed: (!empty()), function front"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
#keyPath(
print
