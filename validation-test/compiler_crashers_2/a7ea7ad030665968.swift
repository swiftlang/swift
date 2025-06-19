// {"signature":"swift::TypeChecker::checkObjCKeyPathExpr(swift::DeclContext*, swift::KeyPathExpr*, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
#keyPath(
print
