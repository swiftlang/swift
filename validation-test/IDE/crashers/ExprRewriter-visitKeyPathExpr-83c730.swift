// {"kind":"complete","original":"e7b7b804","signature":"(anonymous namespace)::ExprRewriter::visitKeyPathExpr(swift::KeyPathExpr*)","signatureNext":"ExprWalker::walkToExprPost"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a < b { c (b -> b= \b#^^#
