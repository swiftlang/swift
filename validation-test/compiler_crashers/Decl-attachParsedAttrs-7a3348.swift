// {"kind":"typecheck","original":"cc9b3454","signature":"swift::Decl::attachParsedAttrs(swift::DeclAttributes)","signatureAssert":"Assertion failed: (!OriginalDeclaration || OriginalDeclaration == originalDeclaration && \"Original declaration cannot have already been set\"), function attachToDeclImpl","signatureNext":"Parser::parseDeclVar"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension <#type#> {
  @derivative(of: +) var a, b
}
