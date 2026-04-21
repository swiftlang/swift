// {"kind":"typecheck","original":"63dee018","signature":"swift::rewriting::RequirementMachine::requiresProtocol(swift::Type, swift::ProtocolDecl const*) const","signatureNext":"SubstitutionMap::lookupConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c: a
  struct d<e: a> {
    enum c where c.f == <#type#> {
      typealias f = b
    }
    typealias b = e.c.b
  }
}
