// {"kind":"typecheck","original":"4c49d79d","signature":"(anonymous namespace)::ExprRewriter::convertLiteralInPlace(swift::LiteralExpr*, swift::Type, swift::ProtocolDecl*, swift::Identifier, swift::DeclName, swift::ProtocolDecl*, swift::DeclName, swift::Diag<>, swift::Diag<>)","signatureAssert":"Assertion failed: (protocol && \"requirements should have stopped recursion\"), function convertLiteralInPlace","signatureNext":"ExprRewriter::handleIntegerLiteralExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a < b where b : ExpressibleByIntegerLiteral,
b.IntegerLiteralType == String >() {
  let <#pattern#>:
  b = 2
}
