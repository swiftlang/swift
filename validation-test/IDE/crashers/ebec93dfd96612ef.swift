// {"kind":"complete","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
-switch  {
case
< (&a
#^^#
