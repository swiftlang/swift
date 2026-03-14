// {"kind":"typecheck","original":"3d2e2125","signature":"swift::SubstitutionMap::lookupConformance(swift::CanType, swift::ProtocolDecl*) const"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b where c: d
}
extension a where b == <#type#> {
  typealias c = b
}
