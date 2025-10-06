// {"kind":"complete","original":"3a52f860","signature":"swift::TypeChecker::resolveDeclRefExpr(swift::UnresolvedDeclRefExpr*, swift::DeclContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Sequence where a == b [ { func c { map#^^#
