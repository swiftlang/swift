// {"kind":"typecheck","original":"d3f332e9","signature":"swift::DependentMemberType::get(swift::Type, swift::AssociatedTypeDecl*)","signatureAssert":"Assertion failed: (assocType && \"Missing associated type\"), function get","signatureNext":"InverseRequirement::expandDefaults"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a < b {
  associatedtype b : ~Copyable
}
@freestanding(declaration arbitrary) macro c
struct d
  extension a {
    #c(d
