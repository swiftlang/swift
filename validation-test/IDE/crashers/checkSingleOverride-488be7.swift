// {"kind":"complete","signature":"checkSingleOverride(swift::ValueDecl*, swift::ValueDecl*)","signatureAssert":"Assertion failed: (!baseName.isSpecial() && \"Can't retrieve the identifier of a special base name\"), function getBaseIdentifier"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a { subscript(a) a { set } class b : a { override subscript(a) a {
#^COMPLETE^#
