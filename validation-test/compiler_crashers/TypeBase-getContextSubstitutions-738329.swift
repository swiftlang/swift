// {"kind":"typecheck","original":"2673c5be","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (0 && \"Bad base type\"), function getContextSubstitutions","signatureNext":"TypeResolution::applyUnboundGenericArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b<c> {
    typealias d<e> = <#type#>
  }
  extension a where Self: b {
    func
      f<e: d<e>>()
  }
}
