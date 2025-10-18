// {"kind":"complete","original":"e6459edc","signature":"swift::TypeChecker::typeCheckStmtConditionElement(swift::StmtConditionElement&, bool&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!E->getType() && \"the bool condition is already type checked\"), function typeCheckBooleanStmtConditionElement"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
(if) {}#^^#
