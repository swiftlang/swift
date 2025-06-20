// {"signature":"cloneRawLiteralExpr(swift::ASTContext&, swift::LiteralExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a enum a : Double { case = 3.7 case
