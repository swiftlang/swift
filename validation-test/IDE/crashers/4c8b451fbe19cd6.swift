// {"kind":"complete","signature":"(anonymous namespace)::PreCheckTarget::simplifyTypeExpr(swift::Expr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
macro a = { #^COMPLETE^#
