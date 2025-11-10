// {"kind":"typecheck","original":"115aa383","signature":"swift::TypeChecker::lookupUnqualified(swift::DeclContext*, swift::DeclNameRef, swift::SourceLoc, swift::optionset::OptionSet<swift::NameLookupFlags, unsigned int>)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b<c {
    d    extension Never {
      struct e {
b {
  d
