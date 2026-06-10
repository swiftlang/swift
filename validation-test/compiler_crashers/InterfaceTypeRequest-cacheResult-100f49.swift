// {"kind":"typecheck","original":"2b74e212","signature":"swift::InterfaceTypeRequest::cacheResult(swift::Type) const","signatureAssert":"Assertion failed: (decl->getDeclContext()->isLocalContext() || !type->hasLocalArchetype() && \"Local archetype in interface type of non-local declaration\"), function cacheResult","signatureNext":"ValueDecl::getInterfaceType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<b {
  associatedtype b
  c
  d->
    a<b>
  associatedtype e
  where f == b
}
typealias g =
  a
let h: g
let i =
  h.d
