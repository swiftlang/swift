// {"kind":"complete","signature":"(anonymous namespace)::PreCheckTarget::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: ((closure->getParent() == DC || closure->getParent()->isChildContextOf(DC)) && \"Decl context isn't correct\"), function walkToClosureExprPre"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a = { #^COMPLETE^#
