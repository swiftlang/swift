// {"kind":"complete","signature":"swift::IterableDeclContext::addMemberSilently(swift::Decl*, swift::Decl*, bool) const::$_0::operator()(swift::Decl*, swift::Decl*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
class a {b { { [ } ] in
#^COMPLETE^#
}
c)
