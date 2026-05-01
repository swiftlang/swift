// {"kind":"typecheck","original":"b2c5f36c","signature":"swift::ArgumentList::getSourceRange() const","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange","signatureNext":"Expr::getLoc"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a {
  var values: <#type#>
  :
  subscript<b>(dynamicMember keyPath: KeyPath<[String: Any], b>) -> b {
    values[keyPath: keyPath]
    c
  }
}
