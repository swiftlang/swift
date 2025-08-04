// {"kind":"typecheck","signature":"cloneRawLiteralExpr(swift::ASTContext&, swift::LiteralExpr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a enum a : Double { case = 3.7 case
