// {"kind":"typecheck","original":"1157c215","signature":"makeBinOp(swift::ASTContext&, swift::Expr*, swift::Expr*, swift::Expr*, swift::PrecedenceGroupDecl*, bool)"}
// RUN: not %target-swift-frontend -typecheck %s
{
  do 
catch _ as a + {
  }
}
