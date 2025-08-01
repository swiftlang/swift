// {"kind":"typecheck","signature":"(anonymous namespace)::SyntacticElementSolutionApplication::visitReturnStmt(swift::ReturnStmt*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a func b(c : a) {
let:
  ()->Copyable = { c return
