// {"kind":"complete","signature":"swift::IterableDeclContext::addMemberSilently(swift::Decl*, swift::Decl*, bool) const","signatureNext":"IterableDeclContext::addMember"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {b { { [ } ] in
#^COMPLETE^#
}
c)
