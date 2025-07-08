// {"kind":"complete","signature":"swift::IterableDeclContext::addMemberSilently(swift::Decl*, swift::Decl*, bool) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
{
  class a {
    lazy b: = #^COMPLETE^#
