// {"kind":"typecheck","original":"8fe2aedb","signature":"makeBinOp(swift::ASTContext&, swift::Expr*, swift::Expr*, swift::Expr*, swift::PrecedenceGroupDecl*, bool)"}
// RUN: not %target-swift-frontend -typecheck %s
{
  (a: b) in
  switch a {
  case c -> _:
  }
}
