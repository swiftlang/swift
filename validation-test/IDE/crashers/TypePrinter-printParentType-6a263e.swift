// {"kind":"complete","original":"68dfc9c1","signature":"(anonymous namespace)::TypePrinter::printParentType(swift::Type)","signatureAssert":"Assertion failed: (!hasTypeParameter() && \"already have an interface type\"), function mapTypeOutOfEnvironment","signatureNext":"TypePrinter::visitBoundGenericType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b { struct c<d { struct e<f { typealias e<g> = c<g>.e<g>#^^#
