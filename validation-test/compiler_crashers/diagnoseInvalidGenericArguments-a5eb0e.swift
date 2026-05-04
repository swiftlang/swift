// {"kind":"typecheck","original":"71b7ae11","signature":"swift::diagnoseInvalidGenericArguments(swift::SourceLoc, swift::ValueDecl*, unsigned int, unsigned int, bool, swift::SourceRange)","signatureAssert":"Assertion failed: (!baseName.isSpecial() && \"Can't retrieve the identifier of a special base name\"), function getBaseIdentifier","signatureNext":"InvalidTypeSpecializationArity::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a< b > {
  subscript <c>(dynamicMember d: WritableKeyPath<b, c>) -> a {
    a< >
  }
  var
    a: <#type#>
}
