// {"kind":"complete","original":"4eb391c3","signature":"(anonymous namespace)::AttributeChecker::visitCustomAttr(swift::CustomAttr*)","signatureAssert":"Assertion failed: (Ctx.hadError()), function visitCustomAttr","signatureNext":"TypeChecker::checkDeclAttributes","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
#^^# typealias a = .struct b {
@a c
#if 0#^d^#
