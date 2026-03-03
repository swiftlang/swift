// {"kind":"complete","signature":"swift::IterableDeclContext::addMemberSilently(swift::Decl*, swift::Decl*, bool) const","signatureAssert":"Assertion failed: (hint == nullptr), function addMemberSilently"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  class a {
    lazy b: = #^COMPLETE^#
