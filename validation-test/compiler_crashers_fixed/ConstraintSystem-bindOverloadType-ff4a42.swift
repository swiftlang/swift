// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::bindOverloadType(swift::constraints::SelectedOverload const&, swift::Type, swift::constraints::ConstraintLocator*, swift::DeclContext*)","signatureAssert":"Assertion failed: (constraints.size() == 1), function bindOverloadType","signatureNext":"ConstraintSystem::resolveOverload"}
// RUN: not %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a {
  subscript(dynamicMember c: KeyPath<Int, Int>) -> Int
  func d(b: a) {
    b[e]
  }
}
