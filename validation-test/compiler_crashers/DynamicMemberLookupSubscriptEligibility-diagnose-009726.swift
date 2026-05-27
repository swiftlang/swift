// {"kind":"typecheck","original":"8a3f183e","signature":"swift::DynamicMemberLookupSubscriptEligibility::diagnose(swift::SubscriptDecl*) const","signatureAssert":"Assertion failed: (diagnosed), function diagnose","signatureNext":"AttributeChecker::visitDynamicMemberLookupAttr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup class a {
  subscript( String, b )
