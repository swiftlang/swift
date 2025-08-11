// {"kind":"complete","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a }
{
var b: a func c { switch b {
#^COMPLETE^#
