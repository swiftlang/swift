// {"kind":"complete","original":"d26d12fc","signature":"(anonymous namespace)::TypePrinter::printParentType(swift::Type)","signatureAssert":"Assertion failed: (!hasTypeParameter() && \"already have an interface type\"), function mapTypeOutOfEnvironment","signatureNext":"TypePrinter::visitTypeAliasType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Sequence { a<b , c: KeyPath<Element, b>>(d: c) { [d:b] []#^^#
