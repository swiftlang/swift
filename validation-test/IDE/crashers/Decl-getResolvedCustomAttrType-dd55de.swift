// {"kind":"complete","original":"1146522a","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a { @b c = subscript (d) e
#^^#
