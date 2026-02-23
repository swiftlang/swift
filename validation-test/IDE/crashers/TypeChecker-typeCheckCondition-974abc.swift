// {"kind":"complete","original":"e6459edc","signature":"swift::TypeChecker::typeCheckCondition(swift::Expr*&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!expr->getType() || isa<OpaqueValueExpr>(expr) && \"the bool condition is already type checked\"), function typeCheckCondition"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
(if) {}#^^#
