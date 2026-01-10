// {"kind":"complete","original":"7ab2abac","signature":"swift::DependentMemberType::get(swift::Type, swift::AssociatedTypeDecl*)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
}
extension Sequence where Element: a {
c -> [Element.b]
{
  .c(#^^#
