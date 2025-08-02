// {"kind":"complete","signature":"(anonymous namespace)::PreCheckTarget::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a = { #^COMPLETE^#
