// {"kind":"complete","original":"9aacdbe7","signature":"(anonymous namespace)::StmtChecker::typeCheckASTNode(swift::ASTNode&)","signatureNext":"TypeChecker::typeCheckASTNode"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@resultBuilder struct a {
  static buildPartialBlock(first b
  static buildPartialBlock(accumulated: , next:
  func c<d>(@a body: () -> d
  enum e {
f {
c {
switch {
case let g
#^^#
case
