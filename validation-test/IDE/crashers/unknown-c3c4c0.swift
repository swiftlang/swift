// {"kind":"complete","original":"61c9a362","signature":"unknown"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@dynamicMemberLookup struct a<b> {
  typealias c = a<[b]>
  subscript(
    d:
      #^^#
  ) -> <#type#>
  subscript<e>(dynamicMember f: KeyPath<c, e>) -> e
}
