// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::bindOverloadType(swift::constraints::SelectedOverload const&, swift::Type, swift::constraints::ConstraintLocator*, swift::DeclContext*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<b > {
  subscript <c>(dynamicMember d: WritableKeyPath<b, c>) -> a
  let binding = {
    buffer ?? binding
  } [<#expression#>]
}
