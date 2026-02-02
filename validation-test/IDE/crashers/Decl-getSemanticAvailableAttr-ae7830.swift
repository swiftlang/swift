// {"kind":"complete","original":"0715e0e8","signature":"swift::Decl::getSemanticAvailableAttr(swift::AvailableAttr const*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {
  @available(*, renamed: "request0")
  !
  b ,() {}
}
#^^#
