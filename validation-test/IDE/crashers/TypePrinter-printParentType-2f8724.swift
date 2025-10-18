// {"kind":"complete","original":"1615843d","signature":"(anonymous namespace)::TypePrinter::printParentType(swift::Type)","signatureAssert":"Assertion failed: (!hasTypeParameter() && \"already have an interface type\"), function mapTypeOutOfContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a<b class c<b , e : a<b>> { class d let builder = d#^^#
