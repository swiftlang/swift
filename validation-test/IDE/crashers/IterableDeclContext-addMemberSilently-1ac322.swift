// {"kind":"complete","signature":"swift::IterableDeclContext::addMemberSilently(swift::Decl*, swift::Decl*, bool) const::$_0::operator()(swift::Decl*, swift::Decl*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {b { { [ } ] in
#^COMPLETE^#
}
c)
