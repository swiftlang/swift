// {"kind":"typecheck","original":"3a29b3d7","signature":"swift::constraints::ConstraintSystem::bindOverloadType(swift::constraints::SelectedOverload const&, swift::Type, swift::constraints::ConstraintLocator*, swift::DeclContext*)","signatureAssert":"Assertion failed: (constraints.size() == 1), function bindOverloadType","signatureNext":"ConstraintSystem::resolveOverload"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<b> {
  subscript<c>(dynamicMember d: KeyPath<b, c>) -> c
  let e = \a[f]
}
