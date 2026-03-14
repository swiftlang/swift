// {"kind":"typecheck","original":"f2b215bc","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (0 && \"Bad base type\"), function getContextSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  extension a where Self: b {
    @G struct b {
      typealias G =
