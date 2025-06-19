// {"signature":"makeBinOp(swift::ASTContext&, swift::Expr*, swift::Expr*, swift::Expr*, swift::PrecedenceGroupDecl*, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
let a= switch a { case &b as? b
