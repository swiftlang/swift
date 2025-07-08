// {"kind":"typecheck","signature":"swift::checkAccessControl(swift::Decl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<b > {
  subscript <c>(dynamicMember d: WritableKeyPath<b, c>) -> a
  let binding = {
    buffer ?? binding
  } [<#expression#>]
}
