// {"kind":"typecheck","original":"71b7ae11","signature":"swift::diagnoseInvalidGenericArguments(swift::SourceLoc, swift::ValueDecl*, unsigned int, unsigned int, bool, swift::SourceRange)","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier","signatureNext":"InvalidTypeSpecializationArity::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a< b > {
  subscript <c>(dynamicMember d: WritableKeyPath<b, c>) -> a {
    a< >
  }
  var
    a: <#type#>
}
