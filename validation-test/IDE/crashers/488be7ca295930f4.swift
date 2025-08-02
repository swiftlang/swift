// {"kind":"complete","signature":"checkSingleOverride(swift::ValueDecl*, swift::ValueDecl*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a { subscript(a) a { set } class b : a { override subscript(a) a {
#^COMPLETE^#
