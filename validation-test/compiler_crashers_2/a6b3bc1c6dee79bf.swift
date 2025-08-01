// {"kind":"typecheck","original":"901f9c6d","signature":"bool swift::constraints::isExpr<swift::MemberRefExpr>(swift::ASTNode)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
let a = [].reduce([]) {
  0 && a($1 == a
  $1 {
